open Syntax
open Types

printfn "input file path:"
// let path = System.Console.ReadLine()
let path = "sample/sample.phis" // DEBUG

printfn "output file path:"
// let outputPath = System.Console.ReadLine()
let outputPath = "sample/sample.is" // DEBUG

let source = System.IO.File.ReadAllText path

let parser = Parser.program
let ast = source |> FParsec.CharParsers.run parser

let rec printTypedAst (typingState: Type.TypingState) (ast: Syntax.Statement seq) =
    Seq.fold printTypedStatement typingState ast

and printTypedStatement (typingState: Type.TypingState) (statement: Syntax.Statement) =
    match statement with
    | Let(Pattern.Variable(var, ty), expr) ->
        let ty' =
            Type.Expr.typing typingState expr
            |> Result.defaultWith (sprintf "ERROR: %A" >> failwith)

        let ty'' =
            if ty.IsNone then Ok ty'
            elif Type.isCompatible ty' ty.Value then Ok ty.Value
            else Error(sprintf "%A is not subtype of %A" ty' ty.Value)

        match ty'' with
        | Ok ty'' ->
            printfn "let %s: %A = %A" var.Compose ty'' "..." // DEBUG: expr
            Type.TypingState.addVar var ty'' Type.Mutability.Immutable typingState
        | Error msg ->
            printfn "let %s: %A = %A // ERROR: %s" var.Compose ty.Value "..." msg // DEBUG: expr
            typingState
    | Let(_, _) ->
        printfn "let _ = _ // ERROR: Not Implemented"
        typingState
    | Var(Pattern.Variable(var, ty), expr) ->
        let ty' =
            Type.Expr.typing typingState expr
            |> Result.defaultWith (sprintf "ERROR: %A" >> failwith)
            |> Type.widen

        let ty'' =
            if ty.IsNone then Ok ty'
            elif Type.isCompatible ty' ty.Value then Ok ty.Value
            else Error(sprintf "%A is not subtype of %A" ty' ty.Value)

        match ty'' with
        | Ok ty'' ->
            printfn "var %s: %A = %A" var.Compose ty'' "..." // DEBUG: expr
            Type.TypingState.addVar var ty'' Type.Mutability.Mutable typingState
        | Error msg ->
            printfn "var %s: %A = %A // ERROR: %s" var.Compose ty.Value "..." msg // DEBUG: expr
            typingState
    | Var(_, _) ->
        printfn "var _ = _ // ERROR: Not Implemented"
        typingState
    | Gets(Pattern.Variable(var, ty), expr) ->
        let ty' = Type.Expr.typing typingState expr |> Result.defaultValue Type.Any

        if ty.IsNone || Type.isCompatible ty' ty.Value then
            printfn "%s <- %A" var.Compose expr
        else
            printfn "%s <- %A // ERROR: %A is not subtype of %A" var.Compose expr ty' ty

        typingState
    | Gets(_, _) ->
        printfn "_ = _ // ERROR: Not Implemented"
        typingState
    | Do expr
    | RawExpr expr ->
        let ty = Type.Expr.typing typingState expr

        match ty with
        | Ok ty -> printfn "do %A: %A" expr ty
        | Error msg -> printfn "do %A // ERROR: %A" expr msg

        typingState
    | For(Pattern.Variable(var, ty), range, statements) ->
        printfn "for %A: %A in %A" var (ty |> Option.defaultValue Type.Int) range
        let _ = printTypedAst typingState statements
        printfn "end // for"
        typingState
    | For(pat, range, statements) ->
        printfn "for %A in %A // ERROR: Not Implemented" pat range
        let _ = printTypedAst typingState statements
        typingState
    | Return expr ->
        printfn "return %A" expr
        typingState

match ast with
| FParsec.CharParsers.Failure(msg, _, _) -> printfn "FAILED!\n%s" msg
| FParsec.CharParsers.Success(ast, _, _) ->
    let typingState =
        Type.TypingState.empty
        |> Type.TypingState.addVar
            (Var.Namespace { Path = [ "Core" ]; Name = "add" })
            (Type.Function([ Type.Some; Type.Some ], Type.Any)) // External function.
            Type.Mutability.Immutable
        |> Type.TypingState.addUnOp { Name = UnOpName "<:" } Type.Some Type.Void
        |> Type.TypingState.addBinOp { Name = BinOpName "==" } Type.Some Type.Some Type.Bool
        |> Type.TypingState.addBinOp { Name = BinOpName "<" } Type.Number Type.Number Type.Bool
        |> Type.TypingState.addBinOp { Name = BinOpName "+" } Type.Int Type.Int Type.Int

    printTypedAst typingState ast |> ignore

    let _ =
        (Ok typingState, ast)
        ||> Seq.fold (fun state statement ->
            state |> Result.bind (fun state -> Type.Expr.typingStatement state statement))

    let transpiled = Transpiler.transpile ast
    System.IO.File.WriteAllText(outputPath, transpiled)
    printfn "Succeeded."
