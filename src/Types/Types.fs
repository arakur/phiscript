namespace Types

[<RequireQualifiedAccess>]
type LiteralType =
    | Numeral of Syntax.Numeral
    | StringLit of Syntax.StringLit
    | True
    | False

    static member fromSyntax(literal: Syntax.LiteralType) =
        match literal with
        | Syntax.LiteralType.Numeral numeral -> Numeral numeral
        | Syntax.LiteralType.StringLit stringLit -> StringLit stringLit
        | Syntax.LiteralType.True -> True
        | Syntax.LiteralType.False -> False

[<RequireQualifiedAccess>]
type Type =
    | Literal of LiteralType
    | Int
    | Number
    | String
    | Bool
    | Null
    | SizedArray of Type list
    | Array of Type
    | Object of Map<Syntax.Key, Type>
    | Union of lhs: Type * rhs: Type
    | Function of args: Type list * ret: Type
    | Any
    | Some

module Literal =
    let typeOf (lit: LiteralType) =
        match lit with
        | LiteralType.Numeral numeral -> if numeral.IsInt then Type.Int else Type.Number
        | LiteralType.StringLit _ -> Type.String
        | LiteralType.True
        | LiteralType.False -> Type.Bool
