type DigitSeq =
    | DigitSeq of digits: string

    static member unwrap(DigitSeq digits) = digits

    member this.Unwrap = DigitSeq.unwrap this

type Numeral =
    { Integer: DigitSeq
      Decimal: DigitSeq option }

    static member mkInt integer = { Integer = integer; Decimal = None }

    static member mkFloat integer decimal =
        { Integer = integer
          Decimal = Some decimal }

    static member mk integer decimal =
        { Integer = integer; Decimal = decimal }

    static member compose(this: Numeral) =
        match this.Decimal with
        | None -> this.Integer.Unwrap
        | Some decimal -> this.Integer.Unwrap + "." + decimal.Unwrap

    member this.Compose = Numeral.compose this

type StringLit =
    | StringLit of content: string

    static member compose(StringLit content) = content |> (sprintf "\"%s\"")

    member this.Compose = StringLit.compose this

type VarName =
    { Name: string }

    static member mk name = { Name = name }

type NamespaceVarName =
    { Path: string list
      Name: string }

    static member mk path name = { Path = path; Name = name }

[<RequireQualifiedAccess>]
type Var =
    | NoNamespace of VarName
    | Namespace of NamespaceVarName

    static member compose(this: Var) =
        match this with
        | NoNamespace name -> name.Name
        | Namespace name -> name.Path |> String.concat ":" |> (fun path -> path + ":" + name.Name)

    member this.Compose = Var.compose this

type UnOpName =
    | UnOpName of string

    static member unwrap(UnOpName name) = name

    member this.Unwrap = UnOpName.unwrap this

type BinOpName = BinOpName of string

type UnOp =
    { Name: UnOpName }

    static member mk name = { Name = name }

    static member compose this = this.Name.Unwrap

    member this.Compose = UnOp.compose this

type BinOp =
    { Name: BinOpName }

    static member mk names = { Name = names }

    static member compose(this: BinOp) =
        match this.Name with
        | BinOpName name -> name

    member this.Compose = BinOp.compose this

[<RequireQualifiedAccess>]
type Pattern =
    | Numeral of Numeral
    | StringLit of StringLit
    | Variable of Var
    | Array of Pattern list
    | Tuple of Pattern list
    | Wildcard

[<RequireQualifiedAccess>]
type Expr =
    | Numeral of Numeral
    | StringLit of StringLit
    | Variable of Var
    | Array of Expr list
    | Tuple of Expr list
    | UnOp of UnOp
    | UnOpApplied of op: UnOp * arg: Expr
    | BinOp of BinOp
    | BinOpApplied of op: BinOp * lhs: Expr * rhs: Expr
    | Apply of fun_: Expr * args: Expr list
    | Lambda of pat: Pattern * body: Expr
    | EvalBlock of Block
    | If of cases: (Expr * Block) list * else_: Block option
    | Match of expr: Expr * cases: (Pattern * Block) list

and Statement =
    | Let of pat: Pattern * expr: Expr
    | Var of pat: Pattern * expr: Expr
    | Gets of pat: Pattern * expr: Expr
    | Do of expr: Expr
    | For of pat: Pattern * range: Expr * block: Block
    | Return of expr: Expr
    | RawExpr of expr: Expr

and Block =
    { Statements: Statement list
      Return: Expr option }

    static member mk statements =
        match statements |> List.rev with
        | [] -> { Statements = []; Return = None }
        | RawExpr expr :: statements'
        | Return expr :: statements' ->
            { Statements = statements' |> List.rev
              Return = Some expr }
        | _ ->
            { Statements = statements
              Return = None }

//

let keywords =
    [ "let"; "var"; "do"; "return"; "if"; "elif"; "else"; "match"; "for"; "end" ]
    |> Set.ofList

module Operators =
    open FSharpPlus
    open FSharpx.Collections

    let unOps = [ "+"; "-"; "<:" ]

    let binOpChars = "!#$%&*+-/:<=>?@^|"

    [<RequireQualifiedAccess>]
    type Associativity =
        | Left
        | Right
        | None

    type OperatorsLayer =
        { Names: BinOp Set
          Associativity: Associativity }

    let operatorTable =
        [ { Names =
              [ "="; "<"; ">"; "<="; ">="; "<>"; "!="; "==" ]
              |> List.map BinOpName
              |> List.map BinOp.mk
              |> Set.ofList
            Associativity = Associativity.None }
          { Names = [ "+"; "-" ] |> List.map BinOpName |> List.map BinOp.mk |> Set.ofList
            Associativity = Associativity.Left }
          { Names = [ "*"; "/" ] |> List.map BinOpName |> List.map BinOp.mk |> Set.ofList
            Associativity = Associativity.Left } ]

    type OpSeq =
        { head: Expr
          tail: (BinOp * Expr) Deque }

        static member mk head tail = { head = head; tail = tail }

        static member single expr = OpSeq.mk expr Deque.empty

        member this.AddTerm (op: BinOp) (expr: Expr) =
            OpSeq.mk this.head (this.tail |> Deque.conj (op, expr))

        member this.Reverse =
            match this.tail |> Deque.rev |> Deque.tryUncons with
            | None -> this
            | Some((headOp', head'), tail') ->
                let tailOps' =
                    tail' |> Seq.map fst |> (fun tailOps' -> tailOps' |> Seq.cons headOp')

                let tailExprs' = tail' |> Seq.map snd |> (Seq.append [ this.head ])
                let tail'' = Seq.zip tailOps' tailExprs' |> Deque.ofSeq

                OpSeq.mk head' tail''

        member this.TryHeadTail =
            this.tail
            |> Deque.tryUncons
            |> Option.map (fun ((headOp, head'), tail') -> (this.head, headOp), OpSeq.mk head' tail')

        member this.CollectBinOps(operators: BinOp Set) =
            let rec collect (stack: OpSeq) (ret: (OpSeq * BinOp) list) (currentTail: (BinOp * Expr) Deque) =
                match currentTail |> Deque.tryUncons with
                | None -> ret |> List.rev, stack
                | Some((headOp', head'), tail') ->
                    if operators |> Set.contains headOp' then
                        let ret' = (stack, headOp') :: ret
                        let stack' = OpSeq.single head'
                        collect stack' ret' tail'
                    else
                        let stack' = stack.AddTerm headOp' head'

                        collect stack' ret tail'

            collect (OpSeq.single this.head) [] this.tail

        member this.FoldRight: Expr =
            match this.TryHeadTail with
            | None -> this.head
            | Some((head, headOp), tail) ->
                let tail' = tail.FoldRight
                Expr.BinOpApplied(op = headOp, lhs = head, rhs = tail')

        member this.FoldLeft: Expr =
            let rec foldRev (rev: OpSeq) =
                match rev.TryHeadTail with
                | None -> rev.head
                | Some((head, headOp), tail) ->
                    let tail' = tail.FoldLeft
                    Expr.BinOpApplied(op = headOp, lhs = tail', rhs = head)

            foldRev this.Reverse

        member this.FoldNone: Expr option =
            match this.TryHeadTail with
            | None -> Some this.head
            | Some((lhs, op), tail) ->
                match tail.TryHeadTail with
                | None -> Some(Expr.BinOpApplied(op = op, lhs = lhs, rhs = tail.head))
                | Some _ -> None

    let rec fold (operatorTable: OperatorsLayer list) (opSeq: OpSeq) : Expr option = // TODO: Handle errors.
        match operatorTable with
        | [] ->
            match opSeq.TryHeadTail with
            | None -> Some opSeq.head
            | Some _ -> None
        | { Names = operators
            Associativity = associativity } :: tail ->
            monad {
                let init, last = opSeq.CollectBinOps operators

                let! init' =
                    init
                    |> List.map (fun (opSeq, op) -> op, fold tail opSeq)
                    |> List.map (fun (op, expr) -> expr |> Option.map (fun expr -> op, expr))
                    |> List.fold
                        (fun acc x ->
                            monad {
                                let! acc = acc
                                let! x = x
                                x :: acc
                            })
                        (Some [])

                let! last' = fold tail last

                let opSeq' = (OpSeq.mk last' (init' |> Deque.ofSeq |> Deque.rev)).Reverse

                match associativity with
                | Associativity.Left -> return opSeq'.FoldLeft
                | Associativity.Right -> return opSeq'.FoldRight
                | Associativity.None -> return! opSeq'.FoldNone
            }



module Parser =
    open FParsec
    open FSharpx.Collections

    let private whitespace: Parser<unit, unit> = anyOf " \t" |> many >>% ()

    let private linebreak: Parser<unit, unit> =
        eof <|> (many1 (attempt (whitespace >>. anyOf ";\r\n")) >>% ())

    let private keyword (name: string) : Parser<unit, unit> = whitespace >>. pstring name >>% ()

    let private syntaxSymbol (name: string) : Parser<unit, unit> = whitespace >>. pstring name >>% ()

    let private digitSeq: Parser<DigitSeq, unit> = many1Satisfy isDigit |>> DigitSeq

    let private intNumeral: Parser<Numeral, unit> = digitSeq |>> Numeral.mkInt

    let private floatNumeral: Parser<Numeral, unit> =
        let integer: Parser<DigitSeq, unit> = digitSeq
        let decimal: Parser<DigitSeq, unit> = pchar '.' >>. digitSeq
        pipe2 integer decimal Numeral.mkFloat

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
            if keywords |> Set.contains s then
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

//

module Transpiler =
    let rec private transpilePattern (pattern: Pattern) =
        match pattern with
        | Pattern.Numeral numeral -> numeral.Compose
        | Pattern.StringLit stringLit -> stringLit.Compose
        | Pattern.Variable var -> var.Compose
        | Pattern.Array array ->
            array
            |> List.map transpilePattern
            |> String.concat ", "
            |> (fun s -> "[" + s + "]")
        | Pattern.Tuple tuple -> failwith "Not Implemented"
        | Pattern.Wildcard -> "*"

    let rec private transpileExpr (expr: Expr) =
        match expr with
        | Expr.Numeral numeral -> numeral.Compose
        | Expr.StringLit stringLit -> stringLit.Compose
        | Expr.Variable var -> var.Compose
        | Expr.Array array -> array |> List.map transpileExpr |> String.concat " " |> (fun s -> "[" + s + "]")
        | Expr.Tuple tuple -> tuple |> List.map transpileExpr |> String.concat " " |> (fun s -> "[" + s + "]")
        | Expr.UnOp(_) -> failwith "Not Implemented"
        | Expr.UnOpApplied(op, arg) -> [ op.Compose; "(" + transpileExpr arg + ")" ] |> String.concat " "
        | Expr.BinOp(_) -> failwith "Not Implemented"
        | Expr.BinOpApplied(op, lhs, rhs) ->
            [ "(" + transpileExpr lhs + ")"; op.Compose; "(" + transpileExpr rhs + ")" ]
            |> String.concat " "
        | Expr.Apply(fun_, args) ->
            // TODO: 現状の実装は別の場所で定義したタプルを入力した場合に正しくならないので，型情報を使って正しくする．
            match args with
            | [ Expr.Tuple tuple ] ->
                let fun_' = transpileExpr fun_

                let args' =
                    tuple
                    |> List.map transpileExpr
                    |> String.concat ", "
                    |> (fun s -> "(" + s + ")")

                fun_' + args'
            | _ ->
                let fun_' = transpileExpr fun_

                let args' =
                    args
                    |> List.map transpileExpr
                    |> List.map (fun s -> "(" + s + ")")
                    |> String.concat ""

                fun_' + args'
        | Expr.EvalBlock block -> [ "eval"; block |> transpileBlock ] |> String.concat " "
        | Expr.If(cases, else_) ->
            match cases with
            | [] -> failwith "Unexpected: if with no cases."
            | (ifCond, ifBlock) :: elifCondBlocks ->
                let if' = [ "if"; ifCond |> transpileExpr; ifBlock |> transpileBlock ]

                let elif' =
                    elifCondBlocks
                    |> List.collect (fun (cond, block) -> [ "elif"; cond |> transpileExpr; block |> transpileBlock ])

                let else' =
                    match else_ with
                    | None -> []
                    | Some block -> [ "else"; block |> transpileBlock ]

                if' @ elif' @ else' |> String.concat " "
        | Expr.Match(expr, cases) -> failwith "Not Implemented"
        | Expr.Lambda(pat, body) ->
            let args =
                match pat with
                | Pattern.Tuple tuple -> tuple |> List.map transpilePattern |> String.concat ", "
                | Pattern.Variable var -> var.Compose
                | Pattern.Wildcard -> "_"
                | _ -> failwith "Not Implemented"

            let body' =
                body
                |> transpileExpr
                |> FSharpPlus.String.split [ "\n" ]
                |> Seq.map (fun s -> "    " + s + "\n")
                |> String.concat ""

            $"@({args}) {{\n" + body' + "}"

    and private transpileStatement (statement: Statement) =
        match statement with
        | Do expr
        | RawExpr expr -> transpileExpr expr
        | Let(pat, expr) ->
            match pat with
            | Pattern.Variable var -> [ "let"; var.Compose; "="; transpileExpr expr ] |> String.concat " "
            | _ -> failwith "Not Implemented"
        | Var(pat, expr) ->
            match pat with
            | Pattern.Variable var -> [ "var"; var.Compose; "="; transpileExpr expr ] |> String.concat " "
            | _ -> failwith "Not Implemented"
        | Gets(pat, expr) ->
            match pat with
            | Pattern.Variable var -> [ var.Compose; "="; transpileExpr expr ] |> String.concat " "
            | _ -> failwith "Not Implemented"
        | For(pat, range, block) ->
            let loop = $"(let {pat |> transpilePattern}, {range |> transpileExpr})"

            [ "for"; loop; block |> transpileBlock ] |> String.concat " "
        | Return expr -> expr |> transpileExpr |> (fun s -> "return " + s)

    and transpileBlock (block: Block) =
        let statements =
            block.Statements
            |> Seq.map transpileStatement
            |> Seq.map (fun s ->
                s
                |> FSharpPlus.String.split [ "\n" ]
                |> Seq.map (fun s -> "    " + s + "\n")
                |> String.concat "")
            |> String.concat ""

        let return_ =
            block.Return
            |> Option.map (fun expr -> expr |> transpileExpr |> (fun s -> "    " + s + "\n"))
            |> Option.defaultValue ""

        "{\n" + statements + return_ + "}"

    let transpile (program: Statement list) =
        program |> Seq.map transpileStatement |> String.concat "\n"

//

open FParsec

let path = "sample/sample.phis"

let source = System.IO.File.ReadAllText path

printfn "%s" source

let parser = Parser.program

let ast = source |> FParsec.CharParsers.run parser

match ast with
| Failure(msg, _, _) -> printfn "FAILED!\n%s" msg
| Success(ast, _, _) ->
    printfn "ast:\n%A" ast

    let transpiled = Transpiler.transpile ast

    printfn "output:\n%s" transpiled
