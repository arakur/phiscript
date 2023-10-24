module Parser

open Syntax

open FParsec
open FSharpx.Collections

let private whitespace: Parser<unit, unit> = anyOf " \t" |> many >>% ()

let private linebreak: Parser<unit, unit> =
    eof <|> (many1 (attempt (whitespace >>. anyOf ";\r\n")) >>% ())

let private keyword (name: string) : Parser<unit, unit> = whitespace >>. pstring name >>% ()

let private syntaxSymbol (name: string) : Parser<unit, unit> = whitespace >>. pstring name >>% ()

let private digitSeq: Parser<DigitSeq, unit> = many1Satisfy isDigit |>> DigitSeq

let private numeral: Parser<Numeral, unit> =
    let integer: Parser<DigitSeq, unit> = attempt digitSeq
    let decimal: Parser<DigitSeq option, unit> = opt (attempt (pchar '.' >>. digitSeq))

    pipe2 integer decimal Numeral.mk

let private stringLit: Parser<StringLit, unit> =
    let doubleQuote = attempt (syntaxSymbol "\"")
    let unescaped = noneOf [ '"'; '\\' ]
    let escapeSequences = [ "\\\\", '\\'; "\\\"", '"'; "\\n", '\n' ]
    let escaped = choice (escapeSequences |> List.map (fun (s, c) -> pstring s >>% c))

    let content = manyChars (unescaped <|> escaped)

    between doubleQuote doubleQuote content |>> StringLit

let private varNameHead: Parser<char, unit> =
    satisfy (fun c -> isLetter c || c = '_')

let private varNameTail: Parser<char, unit> =
    satisfy (fun c -> isLetter c || isDigit c || c = '_')

let private varNameSegment: Parser<string, unit> =
    pipe2 varNameHead (manyChars varNameTail) (fun h t -> string h + t)
    >>= (fun s ->
        if Keywords.keywords |> Set.contains s then
            fail "Invalid variable name."
        else
            preturn s)

let private varName: Parser<VarName, unit> = varNameSegment |>> VarName.mk

let private namespaceVarName: Parser<NamespaceVarName, unit> =
    pipe2 (many1 (attempt (varNameSegment .>> syntaxSymbol "."))) varNameSegment NamespaceVarName.mk

let private var: Parser<Var, unit> =
    whitespace
    >>. choice
        [ attempt namespaceVarName |>> Var.Namespace
          attempt varName |>> Var.NoNamespace ]

let private unOpName: Parser<UnOpName, unit> =
    choice (Operators.unOps |> Seq.map pstring) |>> UnOpName

let private unOp: Parser<UnOp, unit> = whitespace >>. unOpName |>> UnOp.mk

let private binOpName: Parser<BinOpName, unit> =
    anyOf Operators.binOpChars |> many1Chars |>> BinOpName

let private binOp: Parser<BinOp, unit> = whitespace >>. binOpName |>> BinOp.mk

let private pattern: Parser<Pattern, unit> =
    let rec p () =
        parse.Delay(fun () ->
            let numeral = numeral |>> Pattern.Numeral
            let stringLit = stringLit |>> Pattern.StringLit
            let variable = attempt var |>> Pattern.Variable

            let array =
                attempt (syntaxSymbol "[") >>. p ()
                .>>. many (attempt (syntaxSymbol "," >>. p ()))
                .>> syntaxSymbol "]"
                |>> (fun (head, tail) -> head :: tail)
                |>> Pattern.Array

            let tuple =
                attempt (syntaxSymbol "(") >>. p ()
                .>>. many (attempt (syntaxSymbol "," >>. p ()))
                .>> syntaxSymbol ")"
                |>> (fun (head, tail) -> head :: tail)
                |>> Pattern.Tuple

            let wildcard = attempt (syntaxSymbol "_") >>% Pattern.Wildcard

            whitespace
            >>. choice [ numeral; stringLit; attempt variable; array; tuple; wildcard ])

    p ()

let private prim: Parser<Expr, unit> =
    let numeral = numeral |>> Expr.Numeral
    let stringLit = stringLit |>> Expr.StringLit
    let variable = var |>> Expr.Variable

    whitespace >>. choice [ attempt numeral; attempt stringLit; attempt variable ]

let rec private statement
    (binOpSeq: (unit -> Parser<Expr, unit>) -> Parser<Expr, unit>)
    (expr: unit -> Parser<Expr, unit>)
    (block: Parser<Statement, unit> -> Parser<Block, unit>)
    : Parser<Statement, unit> =
    let let_: Parser<Statement, unit> =
        parse.Delay(fun () ->
            let pat = keyword "let" >>. pattern
            let expr = syntaxSymbol "=" >>. expr ()

            pat .>>. expr |>> Let)

    let defAssign: Parser<Statement, unit> =
        parse.Delay(fun () ->
            let pat = attempt pattern
            let expr = attempt (syntaxSymbol ":=") >>. expr ()

            pat .>>. expr |>> Let)

    let var_: Parser<Statement, unit> =
        parse.Delay(fun () ->
            let pat = keyword "var" >>. pattern
            let expr = syntaxSymbol "=" >>. expr ()

            pat .>>. expr |>> Var)

    let gets_: Parser<Statement, unit> =
        parse.Delay(fun () ->
            let pat = pattern
            let expr = (attempt (syntaxSymbol "<-") <|> syntaxSymbol "=") >>. expr ()

            pat .>>. expr |>> Gets)

    let do_: Parser<Statement, unit> =
        parse.Delay(fun () -> keyword "do" >>. expr () |>> Do)

    let for_: Parser<Statement, unit> =
        parse.Delay(fun () ->
            let pat = keyword "for" >>. pattern
            let range = syntaxSymbol "in" >>. binOpSeq expr
            let block = block (statement binOpSeq expr block) .>> keyword "end"

            pipe3 pat range block (fun pat range block -> For(pat, range, block)))

    let return_: Parser<Statement, unit> =
        parse.Delay(fun () ->
            let expr = keyword "return" >>. expr ()

            expr |>> Return)

    let rawExpr: Parser<Statement, unit> = parse.Delay(fun () -> expr () |>> RawExpr)

    parse.Delay(fun () ->
        choice
            [ attempt let_
              attempt var_
              attempt defAssign
              attempt gets_
              attempt do_
              attempt for_
              attempt return_
              rawExpr ])

let private block (statement: Parser<Statement, unit>) : Parser<Block, unit> =
    let statementOrEmpty = choice [ attempt statement |>> Some; whitespace >>% None ]
    parse.Delay(fun () -> many (attempt (statementOrEmpty .>> linebreak)) |>> List.choose id |>> Block.mk)

let rec private binOpSeq (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
    let parenthesized (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () -> attempt (syntaxSymbol "(") >>. expr () .>> syntaxSymbol ")")

    let single (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () -> choice [ parenthesized expr; prim ])

    let exprSeq (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let fn = single expr
            let args = many (attempt (single expr))

            fn .>>. args
            |>> (fun (fn, args) -> if args = [] then fn else Expr.Apply(fn, args)))

    let unOpApplied (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let op = attempt unOp
            let arg = exprSeq expr

            op .>>. arg |>> (fun (op, arg) -> Expr.UnOpApplied(op, arg)))

    let term (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () -> choice [ unOpApplied expr; exprSeq expr ])

    parse.Delay(fun () ->
        let head = term expr
        let tail = many (attempt (binOp .>>. term expr)) |>> Deque.ofSeq
        let opSeq = pipe2 head tail Operators.OpSeq.mk
        let fold = Operators.fold Operators.operatorTable

        let resultFold =
            Option.map preturn >> Option.defaultValue (fail "Invalid operator sequence.")

        opSeq |>> fold >>= resultFold)

let commaSeparated (expr: unit -> Parser<Expr, unit>) : Parser<Expr list, unit> =
    parse.Delay(fun () ->
        let head = binOpSeq expr
        let tail = many (attempt (syntaxSymbol "," >>. binOpSeq expr))
        pipe2 head tail (fun head tail -> head :: tail))

let array_ (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
    parse.Delay(fun () ->
        attempt (syntaxSymbol "[") >>. opt (commaSeparated expr) .>> syntaxSymbol "]"
        |>> Option.defaultValue []
        |>> Expr.Array)

let rec private expr () : Parser<Expr, unit> =
    let statement = statement binOpSeq expr block
    let block = block statement

    let if_ (expr: unit -> Parser<Expr, unit>) (block: Parser<Block, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let if_ = attempt (keyword "if") >>. binOpSeq expr .>>. block
            let elif_ = attempt (keyword "elif") >>. binOpSeq expr .>>. block
            let else_ = attempt (keyword "else") >>. block .>> keyword "end"

            let cases = if_ .>>. many elif_ |>> (fun (if_, elifSeq) -> if_ :: elifSeq)

            cases .>>. opt else_ |>> Expr.If)

    let match_ (expr: unit -> Parser<Expr, unit>) (block: Parser<Block, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let match_ = attempt (keyword "match") >>. expr ()
            let case = pattern .>>. block
            let cases = many1 case .>> keyword "end"

            match_ .>>. cases |>> Expr.Match)

    let lambdaExpr (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let pat = attempt (pattern .>> syntaxSymbol "->")
            let body = expr ()

            parse.Delay(fun () -> pat .>>. body |>> Expr.Lambda))

    //

    let if_ = if_ expr block

    let match_ = match_ expr block

    let block = keyword "begin" >>. block |>> Expr.EvalBlock .>> keyword "end"

    let array_ = array_ expr

    let lambdaExpr = lambdaExpr expr

    let raw =
        commaSeparated expr
        |>> (fun exprs -> if exprs.Length = 1 then exprs.Head else Expr.Tuple exprs)

    parse.Delay(fun () -> whitespace >>. choice [ if_; match_; block; array_; lambdaExpr; raw ])

//

let program = many1 (statement binOpSeq expr block .>> linebreak)
