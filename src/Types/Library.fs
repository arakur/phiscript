module Types

open Syntax

open FSharpPlus

module private Result =
    let sequence (result: Result<'T, 'Error> list) =
        result
        |> List.fold
            (fun acc result ->
                match acc, result with
                | Ok acc, Ok result -> Ok(result :: acc)
                | Ok _, Error error -> Error error
                | Error error, _ -> Error error)
            (Ok [])
        |> Result.map List.rev

// [<RequireQualifiedAccess>]
// type Literal =
//     | Int of int
//     | Number of float
//     | String of string
//     | True
//     | False

// [<RequireQualifiedAccess>]
// type Type =
//     | Literal of Literal
//     | Int
//     | Number
//     | String
//     | Bool
//     | Null
//     | Void
//     | SizedArray of Type list
//     | Array of Type
//     | Dict of Map<Key, Type>
//     | Union of lhs: Type * rhs: Type
//     | Function of args: Type list * ret: Type
//     | Any
//     | Some

module Literal =
    let typeOf (lit: LiteralType) =
        match lit with
        | LiteralType.Numeral numeral -> if numeral.Decimal.IsSome then Type.Number else Type.Int
        | LiteralType.StringLit _ -> Type.String
        | LiteralType.True
        | LiteralType.False -> Type.Bool

//

type TypingError = UndefinedVariable of Var

type Typing = Result<Type, TypingError>

module Type =
    let rec isSubtypeRel (ty0: Type) (ty1: Type) =
        match ty0, ty1 with
        // =======
        //  τ ⪯ τ
        | _ when ty0 = ty1 -> true
        // =========
        //  any ⪯ τ
        | Type.Any, _ -> true
        // ==========
        //  τ ⪯ some
        | _, Type.Some -> true
        //  typeOf(λ) ⪯ τ
        // ===============
        //  λ ⪯ τ
        | Type.Literal lit, _ -> isSubtypeRel (Literal.typeOf lit) ty1
        // ==============
        //  int ⪯ number
        | Type.Int, Type.Number -> true
        //  τ₁ ⪯ σ₁ ... τₙ ⪯ σₙ
        // ==============================
        //  [τ₁, ..., τₙ] ⪯ [σ₁, ..., σₙ]
        | Type.SizedArray tys0, Type.SizedArray tys1 ->
            List.length tys0 = List.length tys1 && List.forall2 isSubtypeRel tys0 tys1
        //  τ₁ ⪯ σ ... τₙ ⪯ σ
        // =====================
        //  [τ₁, ..., τₙ] ⪯ σ[]
        | Type.SizedArray tys0, Type.Array ty1 -> List.forall (isSubtypeRel ty1) tys0
        //  τ ⪯ σ
        // =====================
        //  τ[] ⪯ σ[]
        | Type.Array ty0, Type.Array ty1 -> isSubtypeRel ty0 ty1
        //  τ₁ ⪯ σ    τ₂ ⪯ σ
        // =====================
        //  τ₁ | τ₂ ⪯ σ
        | Type.Union(lhs0, rhs0), _ -> isSubtypeRel lhs0 ty1 && isSubtypeRel rhs0 ty1
        //  τ ⪯ σᵢ
        // =====================
        //  τ ⪯ σ₁ | σ₂
        | _, Type.Union(lhs1, rhs1) -> isSubtypeRel ty0 lhs1 || isSubtypeRel ty0 rhs1
        //  τ₁ ⪯ σ₁ ... τₙ ⪯ σₙ
        // ===================================================================
        //  { k₁:τ₁; ...; kₙ:τₙ; kₙ₊₁:τₙ₊₁; ...; kₘ:τₘ } ⪯ { k₁:σ₁; ...; kₙ:σₙ }
        | Type.Dict tys0, Type.Dict tys1 ->
            tys1
            |> Map.toSeq
            |> Seq.forall (fun (key, ty1) ->
                match tys0.TryFind key with
                | Some ty0 -> isSubtypeRel ty0 ty1
                | None -> false)
        //  σ₁ ⪯ τ₁ ... σₙ ⪯ τₙ    ρ₁ ⪯ ρ₂
        // ===========================================
        //  (τ₁, ..., τₙ) -> ρ₁ ⪯ (σ₁, ..., σₙ) -> ρ₂
        | Type.Function(args0, ret0), Type.Function(args1, ret1) ->
            List.length args0 = List.length args1
            && List.forall2 (fun arg0 arg1 -> isSubtypeRel arg1 arg0) args0 args1
            && isSubtypeRel ret0 ret1
        | _ -> false

    type TypingState =
        { Variables: Map<Var, Type> }

        member this.Find var =
            this.Variables.TryFind var |> Option.toResultWith (UndefinedVariable var)

    module Numeral =
        let typing (numeral: Numeral) =
            if numeral.Decimal.IsSome then
                Ok Type.Number
            else
                Ok Type.Int

    module Expr =
        open Syntax

        let rec typing (state: TypingState) (expr: Expr) =
            match expr with
            // ==============
            //  Γ, x:τ ⊢ x:τ
            | Expr.Variable var -> state.Find var
            // ==================
            //  Γ ⊢ λ: typeOf(λ)
            | Expr.Numeral numeral -> numeral |> Numeral.typing
            | Expr.StringLit _ -> Ok Type.String
            //  Γ ⊢ e₁: τ₁ ... Γ ⊢ eₙ: τₙ
            // ====================================================
            //  Γ ⊢ { k₁: e₁; ...; kₙ: eₙ }: { k₁: τ₁; ...; kₙ: τₙ }
            | Expr.Dictionary dict ->
                dict
                |> List.map (fun (key, value) -> value |> typing state |> Result.map (fun ty -> key, ty))
                |> Result.sequence
                |> Result.map Map.ofList
                |> Result.map Type.Dict
            //  Γ, x₁: τ₁, ..., xₙ: τₙ ⊢ e: ρ
            // ===================================================
            //  Γ ⊢ (x₁: τ₁, ..., xₙ: τₙ) -> e: (τ₁, ..., τₙ) -> ρ
            | Expr.Lambda(args, body) ->
                // TODO: Add argument typing syntax.
                let argTypes =
                    args
                    |> List.choose (function
                        | Pattern.Variable(var, Some ty) -> Some(var, ty)
                        | Pattern.Variable(var, None) -> Some(var, Type.Some)
                        | Pattern.Wildcard _ -> None
                        | _ -> failwith "Not Implemented")
                    |> Map.ofList

                let state' =
                    { state with
                        Variables = state.Variables |> Map.union argTypes }

                monad {
                    let! retType = body |> typing state'
                    return Type.Function(argTypes |> Map.toSeq |> Seq.map snd |> List.ofSeq, retType)
                }
            | Expr.Array array ->
                array
                |> List.map (typing state)
                |> Result.sequence
                |> Result.map Type.SizedArray
            | Expr.FieldAccess(expr, key) ->
                expr
                |> typing state
                >>= function
                    | Type.Dict tys -> tys |> Map.tryFind key |> Option.defaultValue Type.Any |> Ok
                    | Type.Any -> Ok Type.Any
                    | _ -> Error <| failwith "Not Implemented"
            | Expr.UnOp(_) -> failwith "Not Implemented"
            | Expr.UnOpApplied(op, arg) -> failwith "Not Implemented"
            | Expr.BinOp(_) -> failwith "Not Implemented"
            | Expr.BinOpApplied(op, lhs, rhs) -> failwith "Not Implemented"
            | Expr.Apply(fun_, args) -> failwith "Not Implemented"
            | Expr.EvalBlock(_) -> failwith "Not Implemented"
            | Expr.If(cases, else_) -> failwith "Not Implemented"
            | Expr.Match(expr, cases) -> failwith "Not Implemented"
            | Expr.Coerce(expr, ty) ->
                monad {
                    let! exprType = expr |> typing state

                    if isSubtypeRel exprType ty then
                        return ty
                    else
                        return! Error <| failwith "Not Implemented"
                }
            | Expr.As(_, ty) -> Ok ty
