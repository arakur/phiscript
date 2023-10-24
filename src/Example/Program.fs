type DigitSeq = DigitSeq of digits: string

type Numeral =
    { Integer: DigitSeq
      Decimal: DigitSeq option }

    static member mkInt integer = { Integer = integer; Decimal = None }

    static member mkFloat integer decimal =
        { Integer = integer
          Decimal = Some decimal }

    static member mk integer decimal =
        { Integer = integer; Decimal = decimal }

type StringLit = StringLit of content: string

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

type UnOpName = UnOpName of string

type BinOpName = BinOpName of string

type UnOp =
    { Name: UnOpName }

    static member mk name = { Name = name }

type BinOp =
    { Name: BinOpName }

    static member mk names = { Name = names }

[<RequireQualifiedAccess>]
type Pattern =
    | Numeral of Numeral
    | StringLit of StringLit
    | Variable of Var
    | Wildcard

[<RequireQualifiedAccess>]
type Expr =
    | Numeral of Numeral
    | StringLit of StringLit
    | Variable of Var
    | UnOp of UnOp
    | UnOpApplied of op: UnOp * arg: Expr
    | BinOp of BinOp
    | BinOpApplied of op: BinOp * lhs: Expr * rhs: Expr
    | Apply of fun_: Expr * args: Expr list
    | Block of Block
    | If of cases: (Expr * Block) list * else_: Block option
    | Match of expr: Expr * cases: (Pattern * Block) list
    | For of pat: Pattern * range: Expr * block: Block

and Statement =
    | Let of pat: Pattern * expr: Expr
    | Var of pat: Pattern * expr: Expr
    | Do of expr: Expr
    | Return of expr: Expr

and Block =
    { Statements: Statement list
      Return: Expr option }

//

let keywords =
    [ "let"; "var"; "do"; "return"; "if"; "else"; "match"; "for" ] |> Set.ofList

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
        [ { Names = [ "+"; "-" ] |> List.map BinOpName |> List.map BinOp.mk |> Set.ofList
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
    open Microsoft.VisualBasic.CompilerServices

    let keyword (name: string) : Parser<unit, unit> = spaces >>. pstring name >>% ()

    let syntaxSymbol (name: string) : Parser<unit, unit> = spaces >>. pstring name >>% ()

    let digit: Parser<char, unit> = satisfy isDigit

    let digitSeq: Parser<DigitSeq, unit> = many1Satisfy isDigit |>> DigitSeq

    let intNumeral: Parser<Numeral, unit> = digitSeq |>> Numeral.mkInt

    let floatNumeral: Parser<Numeral, unit> =
        let integer: Parser<DigitSeq, unit> = digitSeq
        let decimal: Parser<DigitSeq, unit> = pchar '.' >>. digitSeq
        pipe2 integer decimal Numeral.mkFloat

    let numeral: Parser<Numeral, unit> =
        let integer: Parser<DigitSeq, unit> = digitSeq
        let decimal: Parser<DigitSeq option, unit> = opt (attempt (pchar '.' >>. digitSeq))

        pipe2 integer decimal Numeral.mk

    let stringLit: Parser<StringLit, unit> =
        let doubleQuote = pchar '"'
        let unescaped = noneOf [ '"'; '\\' ]
        let escapeSequences = [ "\\\\", '\\'; "\\\"", '"'; "\\n", '\n' ]
        let escaped = choice (escapeSequences |> List.map (fun (s, c) -> pstring s >>% c))

        let content = manyChars (unescaped <|> escaped)

        content |> between doubleQuote doubleQuote |>> StringLit

    let varNameHead: Parser<char, unit> = satisfy (fun c -> isLetter c || c = '_')

    let varNameTail: Parser<char, unit> =
        satisfy (fun c -> isLetter c || isDigit c || c = '_')

    let varNameSegment: Parser<string, unit> =
        pipe2 varNameHead (manyChars varNameTail) (fun h t -> string h + t)

    let varName: Parser<VarName, unit> = varNameSegment |>> VarName.mk

    let namespaceVarName: Parser<NamespaceVarName, unit> =
        pipe2 (sepBy1 varNameSegment (syntaxSymbol ".")) varNameSegment NamespaceVarName.mk

    let var: Parser<Var, unit> =
        choice [ varName |>> Var.NoNamespace; namespaceVarName |>> Var.Namespace ]

    let unOpName: Parser<UnOpName, unit> =
        choice (Operators.unOps |> Seq.map pstring) |>> UnOpName

    let unOp: Parser<UnOp, unit> = spaces >>. unOpName |>> UnOp.mk

    let binOpName: Parser<BinOpName, unit> =
        anyOf Operators.binOpChars |> many1Chars |>> BinOpName

    let binOp: Parser<BinOp, unit> = spaces >>. binOpName |>> BinOp.mk

    let pattern: Parser<Pattern, unit> =
        let numeral = numeral |>> Pattern.Numeral
        let stringLit = stringLit |>> Pattern.StringLit
        let variable = var |>> Pattern.Variable
        let wildcard = pchar '_' >>% Pattern.Wildcard
        choice [ numeral; stringLit; variable; wildcard ]

    let prim: Parser<Expr, unit> =
        let numeral = numeral |>> Expr.Numeral
        let stringLit = stringLit |>> Expr.StringLit
        let variable = var |>> Expr.Variable

        spaces >>. choice [ attempt numeral; attempt stringLit; attempt variable ]

    let rec single () : Parser<Expr, unit> = choice [ parenthesized (); prim ]

    and parenthesized () : Parser<Expr, unit> =
        attempt (syntaxSymbol "(") >>. binOpSeq .>> syntaxSymbol ")"

    and exprSeq: Parser<Expr, unit> =
        let fn = single ()
        let args = many (attempt (single ()))

        fn .>>. args
        |>> (fun (fn, args) -> if args = [] then fn else Expr.Apply(fn, args))

    and unOpApplied: Parser<Expr, unit> =
        let op = attempt unOp
        let arg = exprSeq

        op .>>. arg |>> (fun (op, arg) -> Expr.UnOpApplied(op, arg))

    and term: Parser<Expr, unit> = choice [ unOpApplied; exprSeq ]

    and binOpSeq: Parser<Expr, unit> =
        let head = term
        let tail = many (attempt (binOp .>>. term)) |>> Deque.ofSeq
        let opSeq = pipe2 head tail Operators.OpSeq.mk
        let fold = Operators.fold Operators.operatorTable

        let resultFold =
            Option.map preturn >> Option.defaultValue (fail "Invalid operator sequence.")

        opSeq |>> fold >>= resultFold

    let expr: Parser<Expr, unit> = binOpSeq

//

let path = "sample/sample.phis"

let source = System.IO.File.ReadAllText path

printfn "%s" source

let text = "+ 1 + 2 * - add 2 1"

let parser = Parser.expr

let expr = text |> FParsec.CharParsers.run parser

printfn "%A" expr
