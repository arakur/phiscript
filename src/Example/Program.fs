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

match ast with
| FParsec.CharParsers.Failure(msg, _, _) -> printfn "FAILED!\n%s" msg
| FParsec.CharParsers.Success(ast, _, _) ->
    let typingState =
        Type.TypingState.empty
        |> Type.TypingState.addVar
            (Var.Namespace { Path = [ "Core" ]; Name = "add" })
            (Type.Function([ Type.Some; Type.Some ], Type.Any)) // External function.
            Type.Mutability.Immutable
        |> Type.TypingState.addUnOp { Name = UnOpName "<:" } Type.Some Type.Null
        |> Type.TypingState.addBinOp { Name = BinOpName "==" } Type.Some Type.Some Type.Bool
        |> Type.TypingState.addBinOp { Name = BinOpName "<" } Type.Number Type.Number Type.Bool
        |> Type.TypingState.addBinOp { Name = BinOpName "+" } Type.Int Type.Int Type.Int

    let _ =
        (Ok typingState, ast)
        ||> Seq.fold (fun state statement ->
            state |> Result.bind (fun state -> Type.Expr.typingStatement state statement))

    let transpiled = Transpiler.transpile ast
    System.IO.File.WriteAllText(outputPath, transpiled)
    printfn "Succeeded."
