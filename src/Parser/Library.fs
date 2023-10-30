﻿module Parser

open Syntax

open FParsec
open FSharpx.Collections

let private whitespace: Parser<unit, unit> = anyOf " \t" |> many >>% ()

let private linebreak: Parser<unit, unit> =
    eof <|> (whitespace >>. anyOf ";\r\n" |> attempt |> many1 >>% ())

let private keyword (name: string) : Parser<unit, unit> = whitespace >>. pstring name >>% ()

let private keyword' (name: string) : Parser<unit, unit> = keyword name |> attempt

let private syntaxSymbol (name: string) : Parser<unit, unit> = whitespace >>. pstring name >>% ()

let private digitSeq: Parser<DigitSeq, unit> = many1Satisfy isDigit |>> DigitSeq

let private numeral: Parser<Numeral, unit> =
    let integer: Parser<DigitSeq, unit> = attempt digitSeq

    let decimal: Parser<DigitSeq option, unit> =
        pchar '.' >>. digitSeq |> attempt |> opt

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
    let pathSegment = attempt (varNameSegment .>> syntaxSymbol "::")
    pipe2 (many1 pathSegment) varNameSegment NamespaceVarName.mk

let private var: Parser<Var, unit> =
    whitespace
    >>. choice
        [ attempt namespaceVarName |>> Var.Namespace
          attempt varName |>> Var.NoNamespace ]

let key = (varName |>> Key.Name) <|> (pint32 |>> Key.Index)

let private unOpName: Parser<UnOpName, unit> =
    choice (Operators.unOps |> Seq.map pstring) |>> UnOpName

let private unOp: Parser<UnOp, unit> = whitespace >>. attempt unOpName |>> UnOp.mk

let private binOpName: Parser<BinOpName, unit> =
    anyOf Operators.binOpChars
    |> many1Chars
    >>= (fun s ->
        let reservedSymbols = [ "."; ":"; ":="; "->"; "<-" ]

        if reservedSymbols |> List.contains s then
            fail "Invalid binary operator name."
        else
            preturn s)
    |>> BinOpName

let private binOp: Parser<BinOp, unit> =
    whitespace >>. attempt binOpName |>> BinOp.mk

let private literalType: Parser<LiteralType, unit> =
    parse.Delay(fun () ->
        let numeral = numeral |>> LiteralType.Numeral
        let stringLit = stringLit |>> LiteralType.StringLit
        let true_ = keyword' "true" >>% LiteralType.True
        let false_ = keyword' "false" >>% LiteralType.False

        whitespace >>. choice [ numeral; stringLit; true_; false_ ])

let rec private type_: Parser<Type, unit> =
    let rec type_' () : Parser<Type, unit> =
        let literalType () = literalType |>> Type.Literal
        let int () = keyword' "int" >>% Type.Int
        let number () = keyword' "number" >>% Type.Number
        let string () = keyword' "string" >>% Type.String
        let bool () = keyword' "bool" >>% Type.Bool
        let void_ () = keyword' "void" >>% Type.Void
        let any () = keyword' "any" >>% Type.Any
        let some () = keyword' "some" >>% Type.Some

        let single () =
            [ literalType; int; number; string; bool; void_; any; some ]
            |> Seq.map parse.Delay
            |> Seq.map attempt
            |> choice

        let parenthesized (type_: unit -> Parser<Type, unit>) () : Parser<Type, unit> =
            syntaxSymbol "(" |> attempt >>. type_ () .>> syntaxSymbol ")"

        let sizedArray (type_: unit -> Parser<Type, unit>) () : Parser<Type, unit> =
            syntaxSymbol "[" |> attempt >>. type_ ()
            .>>. (syntaxSymbol "," >>. type_ () |> attempt |> many)
            .>> syntaxSymbol "]"
            |>> List.Cons
            |>> Type.SizedArray

        let array (type_: unit -> Parser<Type, unit>) () : Parser<Type, unit> =
            let single' =
                [ single; parenthesized type_ ]
                |> Seq.map parse.Delay
                |> Seq.map attempt
                |> choice

            single' .>> syntaxSymbol "[]" |>> Type.Array

        let obj (type_: unit -> Parser<Type, unit>) () : Parser<Type, unit> =
            let key' = whitespace >>. key
            let keyValue = key' .>> syntaxSymbol ":" .>>. type_ ()
            let keyValue' = choice [ attempt keyValue |>> Some; whitespace >>% None ]

            let keyValues =
                keyValue' .>>. (whitespace >>. linebreak >>. keyValue' |> attempt |> many)
                |>> List.Cons
                |>> List.choose id

            syntaxSymbol "{" |> attempt >>. keyValues .>> syntaxSymbol "}"
            |>> Map.ofSeq
            |>> Type.Object

        let function_ (type_: unit -> Parser<Type, unit>) () : Parser<Type, unit> =
            let singleArg =
                [ sizedArray type_; array type_; obj type_; single ]
                |> Seq.map parse.Delay
                |> Seq.map attempt
                |> choice
                |>> List.singleton

            let tupleArg =
                syntaxSymbol "(" |> attempt >>. type_ ()
                .>>. (syntaxSymbol "," >>. type_ () |> attempt |> many)
                .>> syntaxSymbol ")"
                |>> List.Cons

            let arg = [ tupleArg; singleArg ] |> Seq.map attempt |> choice

            arg .>> syntaxSymbol "->" .>>. type_ () |>> Type.Function

        let union (type_: unit -> Parser<Type, unit>) () : Parser<Type, unit> =
            let type_' =
                [ function_ type_
                  sizedArray type_
                  array type_
                  obj type_
                  parenthesized type_
                  single ]
                |> Seq.map parse.Delay
                |> Seq.map attempt
                |> choice

            type_' .>>. (syntaxSymbol "|" >>. type_' |> attempt |> many)
            |>> List.Cons
            |>> Seq.reduce (fun lhs rhs -> Type.Union(lhs, rhs))

        parse.Delay(fun () ->
            whitespace
            >>. ([ union type_'
                   function_ type_'
                   sizedArray type_'
                   array type_'
                   obj type_'
                   parenthesized type_'
                   single ]
                 |> Seq.map parse.Delay
                 |> Seq.map attempt
                 |> choice))

    type_' ()

let private pattern: Parser<Pattern, unit> =
    let rec p () =
        parse.Delay(fun () ->
            let numeral = numeral |>> Pattern.Numeral
            let stringLit = stringLit |>> Pattern.StringLit

            let variable =
                var
                .>>. (syntaxSymbol ":" >>. nextCharSatisfies ((<>) '=') >>. type_ |> attempt |> opt)
                |>> Pattern.Variable

            let array =
                syntaxSymbol "[" |> attempt >>. p ()
                .>>. (syntaxSymbol "," >>. p () |> attempt |> many)
                .>> syntaxSymbol "]"
                |>> List.Cons
                |>> Pattern.Array

            let wildcard = syntaxSymbol "*" |> attempt >>% Pattern.Wildcard

            whitespace >>. choice [ numeral; stringLit; attempt variable; array; wildcard ])

    p ()

let private patternSeq: Parser<Pattern list, unit> =
    parse.Delay(fun () ->
        let single = pattern |>> List.singleton

        let tuple =
            syntaxSymbol "(" |> attempt >>. pattern
            .>>. (syntaxSymbol "," >>. pattern |> attempt |> many)
            .>> syntaxSymbol ")"
            |>> List.Cons

        attempt single <|> tuple)

let private prim: Parser<Expr, unit> =
    let numeral = numeral |>> Expr.Numeral
    let stringLit = stringLit |>> Expr.StringLit
    let variable = var |>> Expr.Variable

    whitespace >>. ([ numeral; stringLit; variable ] |> Seq.map attempt |> choice)

let rec private statement
    (binOpSeq: (unit -> Parser<Expr, unit>) -> Parser<Expr, unit>)
    (expr: unit -> Parser<Expr, unit>)
    (block: Parser<Statement, unit> -> Parser<Block, unit>)
    : Parser<Statement, unit> =
    let keywordAssignment () : Parser<Statement, unit> =
        let let_ = keyword' "let" >>% Let
        let var_ = keyword' "var" >>% Var
        let assignKeyword = [ let_; var_ ] |> Seq.map attempt |> choice
        let assignSymbol = syntaxSymbol "="


        assignKeyword .>>. pattern .>> assignSymbol .>>. expr ()
        |>> (fun ((assign, pat), expr) -> assign (pat, expr))

    let symbolAssignment () : Parser<Statement, unit> =
        let defAssign = syntaxSymbol ":=" >>% Let
        let gets0 = syntaxSymbol "=" >>% Gets
        let gets1 = syntaxSymbol "<-" >>% Gets

        let assignSymbols = [ defAssign; gets0; gets1 ] |> Seq.map attempt |> choice

        attempt (pattern .>>. assignSymbols) .>>. expr ()
        |>> (fun ((pat, assign), expr) -> assign (pat, expr))

    let keywordStatement () : Parser<Statement, unit> =
        let do_ = keyword' "do" >>% Do
        let return_ = keyword' "return" >>% Return
        let keyword = [ do_; return_ ] |> Seq.map attempt |> choice
        parse.Delay(fun () -> keyword .>>. expr () |>> (fun (keyword, expr) -> keyword expr))

    let for_ () : Parser<Statement, unit> =
        let pat = keyword' "for" >>. pattern
        let range = syntaxSymbol "in" >>. binOpSeq expr

        let statementOrEmpty =
            choice [ statement binOpSeq expr block |> attempt |>> Some; whitespace >>% None ]

        let statements =
            statementOrEmpty .>>. (linebreak >>. statementOrEmpty |> attempt |> many)
            |>> List.Cons
            |>> List.choose id
            .>> keyword "end"

        pipe3 pat range statements (fun pat range statements -> For(pat, range, statements))

    let rawExpr () : Parser<Statement, unit> = expr () |>> RawExpr

    parse.Delay(fun () ->
        [ keywordAssignment; keywordStatement; for_; symbolAssignment; rawExpr ]
        |> Seq.map parse.Delay
        |> choice)

let private block (statement: Parser<Statement, unit>) : Parser<Block, unit> =
    let statementOrEmpty = choice [ attempt statement |>> Some; whitespace >>% None ]

    parse.Delay(fun () ->
        statementOrEmpty .>>. many (linebreak >>. statementOrEmpty)
        |>> (fun (head, tail) -> head :: tail)
        |>> List.choose id
        |>> Block.mk)

let rec private binOpSeq (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
    let parenthesized (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () -> attempt (syntaxSymbol "(") >>. expr () .>> syntaxSymbol ")")

    let single (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () -> [ parenthesized expr; prim ] |> choice)

    let single' (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let indexAccess =
                syntaxSymbol "[" >>. expr () .>> syntaxSymbol "]"
                |>> (fun index -> fun expr -> Expr.IndexAccess(expr, index))

            let FieldAccess =
                pchar '.' >>. key |>> (fun key -> fun expr -> Expr.FieldAccess(expr, key))

            let chain = [ indexAccess; FieldAccess ] |> Seq.map attempt |> choice |> many

            let folding (expr: Expr) (chain: Expr -> Expr) = chain expr
            pipe2 (single expr) chain (Seq.fold folding))

    let arg (expr: unit -> Parser<Expr, unit>) : Parser<Expr list, unit> =
        parse.Delay(fun () ->
            let singleArg = single' expr |>> List.singleton

            let tupleArg =
                let head = expr ()
                let tail = syntaxSymbol "," >>. expr () |> attempt |> many
                let content = head .>>. tail |>> List.Cons
                between (syntaxSymbol "(") (syntaxSymbol ")") content

            whitespace >>. ([ tupleArg; singleArg ] |> Seq.map attempt |> choice))

    let exprSeq (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let fn = single' expr
            let args = arg expr |> attempt |> many
            let folding fn arg = Expr.Apply(fn, arg)
            pipe2 fn args (Seq.fold folding))

    let unOpApplied (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let op = attempt unOp
            let arg = exprSeq expr
            op .>>. arg |>> Expr.UnOpApplied)

    let term (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () -> choice [ unOpApplied expr; exprSeq expr ])

    parse.Delay(fun () ->
        let head = term expr
        let tail = binOp .>>. term expr |> attempt |> many |>> Deque.ofSeq
        let opSeq = pipe2 head tail Operators.OpSeq.mk
        let fold = Operators.fold Operators.operatorTable

        let resultFold =
            Option.map preturn >> Option.defaultValue (fail "Invalid operator sequence.")

        opSeq |>> fold >>= resultFold)

let commaSeparated (expr: unit -> Parser<Expr, unit>) : Parser<Expr list, unit> =
    parse.Delay(fun () ->
        let head = binOpSeq expr
        let tail = syntaxSymbol "," >>. binOpSeq expr |> attempt |> many
        head .>>. tail |>> List.Cons)

let array_ (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
    parse.Delay(fun () ->
        syntaxSymbol "[" |> attempt >>. opt (commaSeparated expr) .>> syntaxSymbol "]"
        |>> Option.defaultValue []
        |>> Expr.Array)

let rec private expr () : Parser<Expr, unit> =
    let statement = statement binOpSeq expr block
    let block = block statement

    let if_ (expr: unit -> Parser<Expr, unit>) (block: Parser<Block, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let then_ = keyword' "then" <|> (whitespace >>. linebreak)
            let if_ = keyword' "if" >>. binOpSeq expr .>> then_ .>>. block
            let elif_ = keyword' "elif" >>. binOpSeq expr .>> then_ .>>. block
            let else_ = keyword' "else" >>. block .>> keyword "end"

            let cases = if_ .>>. many elif_ |>> List.Cons

            cases .>>. opt else_ |>> Expr.If)

    let match_ (expr: unit -> Parser<Expr, unit>) (block: Parser<Block, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let match_ = keyword' "match" >>. expr ()
            let case = pattern .>>. block
            let cases = many1 case .>> keyword "end"

            match_ .>>. cases |>> Expr.Match)

    let object (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () ->
            let key' = whitespace >>. key
            let value = syntaxSymbol ":" >>. expr ()

            let pair = key' .>>. value

            let pair' = choice [ attempt pair |>> Some; whitespace >>% None ]

            let pairs =
                pair' .>>. (whitespace >>. linebreak >>. pair' |> attempt |> many)
                |>> List.Cons
                |>> List.choose id

            attempt (syntaxSymbol "{") >>. pairs .>> syntaxSymbol "}" |>> Expr.Object)

    let lambdaExpr (expr: unit -> Parser<Expr, unit>) : Parser<Expr, unit> =
        parse.Delay(fun () -> patternSeq .>> syntaxSymbol "->" |> attempt .>>. expr () |>> Expr.Lambda)

    //

    let if_ = if_ expr block

    let match_ = match_ expr block

    let block = keyword "begin" >>. block |>> Expr.EvalBlock .>> keyword "end"

    let array_ = array_ expr

    let object = object expr

    let lambdaExpr = lambdaExpr expr

    let raw = binOpSeq expr

    parse.Delay(fun () ->

        whitespace >>. choice [ if_; match_; block; array_; object; lambdaExpr; raw ])

//

let program = many1 (statement binOpSeq expr block .>> linebreak) .>> eof
