module Types.TypingState

open Types.Typing

let prelude () =
    TypingState.empty
    |> TypingState.addVar
        (Syntax.Var.Namespace { Path = [ "Core" ]; Name = "add" })
        (Type.Function([ Type.Int; Type.Int ], Type.Int))
        Immutable
    |> TypingState.addVar
        (Syntax.Var.Namespace { Path = [ "Core" ]; Name = "add" })
        (Type.Function([ Type.Number; Type.Number ], Type.Number))
        Immutable
    |> TypingState.addUnOp { Name = Syntax.UnOpName "<:" } Type.Some Type.Null
    |> TypingState.addUnOp { Name = Syntax.UnOpName "+" } Type.Int Type.Int
    |> TypingState.addUnOp { Name = Syntax.UnOpName "+" } Type.Number Type.Number
    |> TypingState.addUnOp { Name = Syntax.UnOpName "-" } Type.Int Type.Int
    |> TypingState.addUnOp { Name = Syntax.UnOpName "-" } Type.Number Type.Number
    |> TypingState.addBinOp { Name = Syntax.BinOpName "==" } Type.Some Type.Some Type.Bool
    |> TypingState.addBinOp { Name = Syntax.BinOpName "!=" } Type.Some Type.Some Type.Bool
    |> TypingState.addBinOp { Name = Syntax.BinOpName "<=" } Type.Number Type.Number Type.Bool
    |> TypingState.addBinOp { Name = Syntax.BinOpName ">=" } Type.Number Type.Number Type.Bool
    |> TypingState.addBinOp { Name = Syntax.BinOpName "<" } Type.Number Type.Number Type.Bool
    |> TypingState.addBinOp { Name = Syntax.BinOpName ">" } Type.Number Type.Number Type.Bool
    |> TypingState.addBinOp { Name = Syntax.BinOpName "+" } Type.Int Type.Int Type.Int
    |> TypingState.addBinOp { Name = Syntax.BinOpName "+" } Type.Number Type.Number Type.Number
    |> TypingState.addBinOp { Name = Syntax.BinOpName "-" } Type.Int Type.Int Type.Int
    |> TypingState.addBinOp { Name = Syntax.BinOpName "-" } Type.Number Type.Number Type.Number
    |> TypingState.addBinOp { Name = Syntax.BinOpName "*" } Type.Int Type.Int Type.Int
    |> TypingState.addBinOp { Name = Syntax.BinOpName "*" } Type.Number Type.Number Type.Number
    |> TypingState.addBinOp { Name = Syntax.BinOpName "/" } Type.Int Type.Int Type.Number
    |> TypingState.addBinOp { Name = Syntax.BinOpName "/" } Type.Number Type.Number Type.Number
    |> TypingState.addBinOp { Name = Syntax.BinOpName "%" } Type.Int Type.Int Type.Int
    |> TypingState.addBinOp { Name = Syntax.BinOpName "%" } Type.Number Type.Number Type.Number
