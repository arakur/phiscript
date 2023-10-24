open Parser
open Transpiler

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
