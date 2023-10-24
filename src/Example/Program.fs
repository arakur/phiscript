let path = "sample/sample.phis"

let source = System.IO.File.ReadAllText path

printfn "%s" source

let parser = Parser.program

let ast = source |> FParsec.CharParsers.run parser

match ast with
| FParsec.CharParsers.Failure(msg, _, _) -> printfn "FAILED!\n%s" msg
| FParsec.CharParsers.Success(ast, _, _) ->
    printfn "ast:\n%A" ast

    let transpiled = Transpiler.transpile ast

    printfn "output:\n%s" transpiled
