module Format

open Syntax

let private priorityPattern (pat: Pattern) =
    match pat with
    | Pattern.Numeral _
    | Pattern.StringLit _
    | Pattern.Array _
    | Pattern.Tuple _
    | Pattern.Variable _
    | Pattern.Wildcard -> 0

let private priorityExpr (expr: Expr) =
    match expr with
    | Expr.Numeral _
    | Expr.StringLit _
    | Expr.Variable _
    | Expr.Array _
    | Expr.Tuple _
    | Expr.Dictionary _
    | Expr.FieldAccess _ -> 0
    | Expr.UnOp _ -> failwith "Not Implemented"
    | Expr.UnOpApplied(op, _) ->
        match op.Compose with
        | "<:" -> 70
        | "+" -> 20
        | "-" -> 20
        | _ -> failwith "Not Implemented"
    | Expr.BinOp _ -> failwith "Not Implemented"
    | Expr.BinOpApplied(op, _, _) ->
        match op.Compose with
        | "*" -> 30
        | "/" -> 30
        | "%" -> 30
        | "+" -> 40
        | "-" -> 40
        | "=" -> 50
        | "<" -> 50
        | ">" -> 50
        | "<=" -> 50
        | ">=" -> 50
        | "==" -> 50
        | "!=" -> 50
        | _ -> failwith "Not Implemented"
    | Expr.Apply _ -> 10
    | Expr.Lambda _ -> 60
    | Expr.EvalBlock _ -> 100
    | Expr.If _ -> 100
    | Expr.Match _ -> 100

let format (program: Statement list) =
    let rec formatPattern (pat: Pattern) =
        match pat with
        | Pattern.Numeral numeral -> numeral.Compose
        | Pattern.StringLit stringLit -> stringLit.Compose
        | Pattern.Array array -> sprintf "[%s]" (array |> List.map formatPattern |> String.concat ", ")
        | Pattern.Tuple tuple -> sprintf "(%s)" (tuple |> List.map formatPattern |> String.concat ", ")
        | Pattern.Variable var -> var.Compose
        | Pattern.Wildcard -> "_"

    let rec formatExpr (expr: Expr) =
        match expr with
        | Expr.Numeral numeral -> numeral.Compose
        | Expr.StringLit stringLit -> stringLit.Compose
        | Expr.Variable var -> var.Compose
        | Expr.Array array -> array |> List.map formatExpr |> String.concat ", " |> sprintf "[%s]"
        | Expr.Tuple tuple -> tuple |> List.map formatExpr |> String.concat ", " |> sprintf "(%s)"
        | Expr.Dictionary dict ->
            dict
            |> List.map (fun (key, value) -> sprintf "%s: %s" (key.Compose) (formatExpr value))
            |> String.concat "\n"
            |> sprintf "{\n%s\n}"
        | Expr.FieldAccess(target, key) ->
            let target' =
                formatExpr target
                |> if priorityExpr target < priorityExpr expr then
                       sprintf "(%s)"
                   else
                       id

            sprintf "%s.%s" target' key.Compose
        | Expr.UnOp(_) -> failwith "Not Implemented"
        | Expr.UnOpApplied(op, arg) ->
            let arg' =
                formatExpr arg
                |> if priorityExpr arg < priorityExpr expr then
                       sprintf "(%s)"
                   else
                       id

            sprintf "%s%s" op.Compose arg'
        | Expr.BinOp(_) -> failwith "Not Implemented"
        | Expr.BinOpApplied(op, lhs, rhs) ->
            let lhs' =
                formatExpr lhs
                |> if priorityExpr lhs < priorityExpr expr then
                       sprintf "(%s)"
                   else
                       id

            let rhs' =
                formatExpr rhs
                |> if priorityExpr rhs < priorityExpr expr then
                       sprintf "(%s)"
                   else
                       id

            sprintf "%s %s %s" lhs' op.Compose rhs'
        | Expr.Apply(fun_, args) ->
            let fun' =
                formatExpr fun_
                |> if priorityExpr fun_ < priorityExpr expr then
                       sprintf "(%s)"
                   else
                       id

            let args' = args |> List.map formatExpr |> String.concat ", "

            sprintf "%s(%s)" fun' args'
        | Expr.Lambda(pat, body) -> failwith "Not Implemented"
        | Expr.EvalBlock(_) -> failwith "Not Implemented"
        | Expr.If(cases, else_) -> failwith "Not Implemented"
        | Expr.Match(expr, cases) -> failwith "Not Implemented"

    let rec formatStatement (statement: Statement) =
        match statement with
        | Let(pat, expr) -> sprintf "let %s = %s" (formatPattern pat) (formatExpr expr)
        | DefAssign(pat, expr) -> sprintf "%s := %s" (formatPattern pat) (formatExpr expr)
        | Var(pat, expr) -> failwith "Not Implemented"
        | Gets(pat, expr) -> failwith "Not Implemented"
        | Do(expr) -> failwith "Not Implemented"
        | For(pat, range, block) -> failwith "Not Implemented"
        | Return(expr) -> failwith "Not Implemented"
        | RawExpr(expr) -> failwith "Not Implemented"

    program |> Seq.map formatStatement |> String.concat "\n"
