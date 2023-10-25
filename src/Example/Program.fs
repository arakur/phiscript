printfn "input file path:"
// let path = System.Console.ReadLine()
let path = "sample/sample.phis" // DEBUG
printfn "%A" path // DEBUG

printfn "output file path:"
// let outputPath = System.Console.ReadLine()
let outputPath = "sample/sample.is" // DEBUG
printfn "%A" outputPath // DEBUG

let formatPath = "sample/formatted.phis" // DEBUG

let source = System.IO.File.ReadAllText outputPath

let parser = Parser.program

let ast = source |> FParsec.CharParsers.run parser

match ast with
| FParsec.CharParsers.Failure(msg, _, _) -> printfn "FAILED!\n%s" msg
| FParsec.CharParsers.Success(ast, _, _) ->
    let formatted = Format.format ast
    System.IO.File.WriteAllText(formatPath, formatted)

    let transpiled = Transpiler.transpile ast
    System.IO.File.WriteAllText(outputPath, transpiled)
    printfn "Succeeded."
