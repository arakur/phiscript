namespace Syntax

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

type Key =
    { name: VarName }

    static member mk name = { name = name }

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
    | Dictionary of (Key * Expr) list
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
module Keywords =
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
