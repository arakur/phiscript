module Transpiler

open Syntax

open FSharpPlus

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
            |> String.split [ "\n" ]
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
            |> String.split [ "\n" ]
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
