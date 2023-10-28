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
    let transpiled = Transpiler.transpile ast
    System.IO.File.WriteAllText(outputPath, transpiled)
    printfn "Succeeded."
