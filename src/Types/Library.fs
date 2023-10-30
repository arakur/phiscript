module Types

open Syntax

open FSharpPlus

module private Result =
    let sequence (result: Result<'T, 'Error> list) =
        result
        |> List.fold
            (fun acc result ->
                monad {
                    let! acc = acc
                    let! result = result
                    return result :: acc
                })
            (Ok [])
        |> Result.map List.rev

    let assertWith (condition: bool) (error: 'Error) =
        if condition then Ok() else Error(error)

module Literal =
    let typeOf (lit: LiteralType) =
        match lit with
        | LiteralType.Numeral numeral -> if numeral.IsInt then Type.Int else Type.Number
        | LiteralType.StringLit _ -> Type.String
        | LiteralType.True
        | LiteralType.False -> Type.Bool

//

type TypingError =
    | UndefinedVariable of Var
    | UndefinedUnOp of UnOp
    | UndefinedBinOp of BinOp
    | UnOpArgumentTypeMismatch of op: UnOp * expected: Type list * actual: Type
    | BinOpArgumentTypeMismatch of op: BinOp * expected: (Type * Type) list * actual: (Type * Type)
    | IncompatibleAssignment of source: Type * target: Type
    | ImmutableVariableReassigned of var: Var
    | NotAFunction of Type
    | ArgumentNumberMismatch of expected: int * actual: int
    | ArgumentTypeMismatch of index: int * expected: Type * actual: Type
    | CannotIndexWith of index: Type * target: Type
    | CannotAccessWith of key: Key * target: Type
    | IfConditionTypeNotCompatibleWithBoolean of Type
    | InvalidForRange of Type
    | IncompatiblePattern of pattern: Pattern * target: Type
    | IncompatibleCoercion of source: Type * target: Type

type Typing = Result<Type, TypingError>

module Type =
    let rec isCompatible (ty0: Type) (ty1: Type) =
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
        | Type.Literal lit, _ -> isCompatible (Literal.typeOf lit) ty1
        // ==============
        //  int ⪯ number
        | Type.Int, Type.Number -> true
        //  τ₁ ⪯ σ₁ ... τₙ ⪯ σₙ
        // ==============================
        //  [τ₁, ..., τₙ] ⪯ [σ₁, ..., σₙ]
        | Type.SizedArray tys0, Type.SizedArray tys1 ->
            List.length tys0 = List.length tys1 && List.forall2 isCompatible tys0 tys1
        //  τ₁ ⪯ σ ... τₙ ⪯ σ
        // =====================
        //  [τ₁, ..., τₙ] ⪯ σ[]
        | Type.SizedArray tys0, Type.Array ty1 -> List.forall (isCompatible ty1) tys0
        //  τ ⪯ σ
        // =====================
        //  τ[] ⪯ σ[]
        | Type.Array ty0, Type.Array ty1 -> isCompatible ty0 ty1
        //  τ₁ ⪯ σ    τ₂ ⪯ σ
        // =====================
        //  τ₁ | τ₂ ⪯ σ
        | Type.Union(lhs0, rhs0), _ -> isCompatible lhs0 ty1 && isCompatible rhs0 ty1
        //  τ ⪯ σᵢ
        // =====================
        //  τ ⪯ σ₁ | σ₂
        | _, Type.Union(lhs1, rhs1) -> isCompatible ty0 lhs1 || isCompatible ty0 rhs1
        //  τ₁ ⪯ σ₁ ... τₙ ⪯ σₙ
        // ===================================================================
        //  { k₁:τ₁; ...; kₙ:τₙ; kₙ₊₁:τₙ₊₁; ...; kₘ:τₘ } ⪯ { k₁:σ₁; ...; kₙ:σₙ }
        | Type.Object tys0, Type.Object tys1 ->
            tys1
            |> Map.toSeq
            |> Seq.forall (fun (key, ty1) ->
                let ty0 = tys0.TryFind key
                ty0.IsSome && isCompatible ty0.Value ty1)
        //  σ₁ ⪯ τ₁ ... σₙ ⪯ τₙ    ρ₁ ⪯ ρ₂
        // ===========================================
        //  (τ₁, ..., τₙ) -> ρ₁ ⪯ (σ₁, ..., σₙ) -> ρ₂
        | Type.Function(args0, ret0), Type.Function(args1, ret1) ->
            List.length args0 = List.length args1
            && List.forall2 isCompatible args1 args0
            && isCompatible ret0 ret1
        | _ -> false

    let widen (ty: Type) =
        match ty with
        | Type.Literal lit -> Literal.typeOf lit
        | _ -> ty

    type Mutability =
        | Mutable
        | Immutable

    type VarType =
        { Type: Type
          Mutability: Mutability }

        static member type_(varType: VarType) = varType.Type

        static member mutability(varType: VarType) = varType.Mutability

    type TypingState =
        { Variables: Map<Var, VarType>
          UnOps: Map<UnOp, (Type * Type) list>
          BinOps: Map<BinOp, (Type * Type * Type) list>
          GlobalReturnTypes: Type Set }

        static member empty =
            { Variables = Map.empty
              UnOps = Map.empty
              BinOps = Map.empty
              GlobalReturnTypes = Set.empty }

        static member addVar (var: Var) (ty: Type) (mutability: Mutability) (this: TypingState) =
            { this with
                Variables = this.Variables |> Map.add var { Type = ty; Mutability = mutability } }

        member this.FindVar var =
            this.Variables.TryFind var |> Option.toResultWith (UndefinedVariable var)

        static member addUnOp (op: UnOp) (arg: Type) (ret: Type) (this: TypingState) =
            let types = this.UnOps |> Map.tryFind op |> Option.defaultValue []

            { this with
                UnOps = this.UnOps |> Map.add op ((arg, ret) :: types) }

        member this.FindUnOp op arg =
            match this.UnOps |> Map.tryFind op with
            | None -> Error(UndefinedUnOp op)
            | Some types ->
                types
                |> Seq.tryFind (fun (arg', _) -> isCompatible arg arg')
                |> Option.map snd
                |> Option.toResultWith (UnOpArgumentTypeMismatch(op, types |> List.map fst, arg))

        static member addBinOp (op: BinOp) (lhs: Type) (rhs: Type) (ret: Type) (this: TypingState) =
            let types = this.BinOps |> Map.tryFind op |> Option.defaultValue []

            { this with
                BinOps = this.BinOps |> Map.add op ((lhs, rhs, ret) :: types) }

        member this.FindBinOp op lhs rhs =
            match this.BinOps |> Map.tryFind op with
            | None -> Error(UndefinedBinOp op)
            | Some types ->
                types
                |> Seq.tryFind (fun (lhs', rhs', _) -> isCompatible lhs lhs' && isCompatible rhs rhs')
                |> Option.map item3
                |> Option.toResultWith (
                    BinOpArgumentTypeMismatch(op, types |> List.map (fun (lhs, rhs, _) -> (lhs, rhs)), (lhs, rhs))
                )

        static member addReturnType (ty: Type) (this: TypingState) =
            { this with
                GlobalReturnTypes = this.GlobalReturnTypes |> Set.add ty }

        member this.ExitScopeInto(parent: TypingState) =
            { parent with
                GlobalReturnTypes = this.GlobalReturnTypes }

    module Numeral =
        let typing (numeral: Numeral) =
            LiteralType.Numeral numeral |> Type.Literal |> Ok

        let widen (numeral: Numeral) =
            match numeral.Decimal with
            | Some _ -> Type.Number |> Ok
            | None -> Type.Int |> Ok

    module Pattern =
        let rec typing (state: TypingState) (pattern: Pattern) =
            match pattern with
            | Pattern.Numeral numeral -> numeral |> Numeral.typing
            | Pattern.StringLit _ -> Ok Type.String
            | Pattern.Variable(_, ty) -> Ok(ty |> Option.defaultValue Type.Any)
            | Pattern.Array array ->
                array
                |> List.map (typing state)
                |> Result.sequence
                |> Result.map Type.SizedArray
            | Pattern.Wildcard -> Ok Type.Any

    module Expr =
        let rec typing (state: TypingState) (expr: Expr) =
            match expr with
            // ==============
            //  Γ, x:τ ⊢ x:τ
            | Expr.Variable var -> state.FindVar var |> Result.map VarType.type_
            // ==================
            //  Γ ⊢ λ: typeOf(λ)
            | Expr.Numeral numeral -> numeral |> Numeral.typing
            | Expr.StringLit _ -> Ok Type.String
            //  Γ ⊢ e₁: τ₁ ... Γ ⊢ eₙ: τₙ
            // ====================================================
            //  Γ ⊢ { k₁: e₁; ...; kₙ: eₙ }: { k₁: τ₁; ...; kₙ: τₙ }
            | Expr.Object obj ->
                obj
                |> List.map (fun (key, value) -> value |> typing state |> Result.map (fun ty -> key, ty))
                |> Result.sequence
                |> Result.map Map.ofList
                |> Result.map Type.Object
            //  Γ, x₁: τ₁, ..., xₙ: τₙ ⊢ e: ρ
            // ===================================================
            //  Γ ⊢ (x₁: τ₁, ..., xₙ: τₙ) -> e: (τ₁, ..., τₙ) -> ρ
            | Expr.Lambda(args, body) ->
                let argTypes: Map<Var, VarType> =
                    args
                    |> List.choose (function
                        | Pattern.Variable(var, ty) -> Some(var, ty |> Option.defaultValue Type.Some) // TODO: Implement type inference to infer type of variable enough precisely.
                        | Pattern.Wildcard -> None
                        | _ -> failwith "Not Implemented")
                    |> List.map (fun (var, ty) -> var, { Type = ty; Mutability = Immutable }) // REMARK: Arguments of lambda are always immutable.
                    |> Map.ofList

                let state' =
                    { state with
                        Variables = state.Variables |> Map.union argTypes }

                monad {
                    let! retType = body |> typing state'

                    return
                        Type.Function(
                            argTypes |> Map.toSeq |> Seq.map snd |> Seq.map VarType.type_ |> List.ofSeq,
                            retType
                        )
                }
            | Expr.Array array ->
                array
                |> List.map (typing state)
                |> Result.sequence
                |> Result.map Type.SizedArray
            | Expr.IndexAccess(expr, index) ->
                monad {
                    let! exprType = expr |> typing state
                    let! indexType = index |> typing state

                    match exprType, indexType with
                    | Type.SizedArray tys, Type.Literal(LiteralType.Numeral numeral) ->
                        let! index = numeral.TryToInt |> Option.toResultWith (CannotIndexWith(indexType, exprType))

                        return!
                            tys
                            |> List.tryItem index
                            |> Option.toResultWith (CannotIndexWith(indexType, exprType))
                    | Type.SizedArray tys, _ when isCompatible indexType Type.Int ->
                        return tys |> List.reduce (curry Type.Union)
                    | Type.Array ty, _ when isCompatible indexType Type.Int -> return ty
                    | Type.Object obj, Type.Literal(LiteralType.StringLit(StringLit content)) ->
                        return
                            obj
                            |> Map.tryFind (Key.Name <| VarName.mk content)
                            |> Option.defaultValue Type.Some
                    | Type.Object _, _ when isCompatible indexType Type.String -> return Type.Some
                    | Type.Object _, _ -> return! Error(CannotIndexWith(indexType, exprType))
                    | _ -> return! Error(CannotIndexWith(indexType, exprType))
                }
            | Expr.FieldAccess(expr, key) ->
                expr
                |> typing state
                |> Result.bind (fun exprTy ->
                    match exprTy with
                    | Type.Object tys -> tys |> Map.tryFind key |> Option.defaultValue Type.Some |> Ok
                    | Type.SizedArray tys ->
                        match key with
                        | Key.Index index ->
                            tys
                            |> List.tryItem index
                            |> Option.toResultWith (CannotIndexWith(Type.Int, Type.SizedArray tys))
                        | _ -> Error(CannotAccessWith(key, exprTy))
                    | Type.Array ty ->
                        match key with
                        | Key.Index _ -> Ok ty
                        | _ -> Error(CannotAccessWith(key, exprTy))
                    | Type.Any -> Ok Type.Any
                    | _ -> Error <| failwith "Not Implemented")
            | Expr.UnOp(_) -> failwith "Not Implemented"
            | Expr.UnOpApplied(op, arg) ->
                monad {
                    let! argType = arg |> typing state

                    let! retType = state.FindUnOp op argType

                    return retType
                }
            | Expr.BinOp(_) -> failwith "Not Implemented"
            | Expr.BinOpApplied(op, lhs, rhs) ->
                monad {
                    let! lhsTy = lhs |> typing state
                    let! rhsTy = rhs |> typing state

                    let! retTy = state.FindBinOp op lhsTy rhsTy

                    return retTy
                }
            | Expr.Apply(fun_, args) ->
                monad {
                    let! funType = fun_ |> typing state

                    match funType with
                    | Type.Function(expectedArgTypes, retType) ->
                        let! argTypes = args |> List.map (typing state) |> Result.sequence

                        do!
                            Result.assertWith
                                (List.length expectedArgTypes = List.length argTypes)
                                (ArgumentNumberMismatch(List.length expectedArgTypes, List.length argTypes))

                        for index, (argTy, expectedTy) in Seq.zip argTypes expectedArgTypes |> Seq.indexed do
                            do!
                                Result.assertWith
                                    (isCompatible argTy expectedTy)
                                    (ArgumentTypeMismatch(index, expectedTy, argTy))

                        return retType
                    | Type.Any -> return Type.Any
                    | _ -> return! Error(NotAFunction funType)
                }
            | Expr.EvalBlock block -> typingBlock state block
            | Expr.If(cases, else_) ->
                let caseType (cond: Expr, block: Block) : Result<Type, TypingError> =
                    cond
                    |> typing state
                    |> Result.bind (fun ty ->
                        if isCompatible ty Type.Bool then
                            Ok block
                        else
                            Error(IfConditionTypeNotCompatibleWithBoolean ty))
                    |> Result.bind (typingBlock state)

                monad {
                    let! cases' = cases |> List.map caseType |> Result.sequence

                    let! cases'' =
                        match else_ with
                        | None -> Ok cases'
                        | Some else_ -> typingBlock state else_ |> Result.map (fun else_ -> else_ :: cases')

                    let retTy = cases' |> Seq.reduce (fun lhs rhs -> Type.Union(lhs, rhs))

                    return retTy
                }
            | Expr.Match(expr, cases) ->
                let caseType (exprTy: Type) (pat: Pattern, block: Block) : Result<Type, TypingError> =
                    monad {
                        let! patTy = pat |> Pattern.typing state
                        let! blockTy = typingBlock state block
                        do! Result.assertWith (isCompatible patTy exprTy) (IncompatiblePattern(pat, exprTy))
                        return blockTy
                    }

                monad {
                    let! ty = expr |> typing state
                    let! cases' = cases |> List.map (caseType ty) |> Result.sequence
                    return cases' |> Seq.reduce (fun lhs rhs -> Type.Union(lhs, rhs))
                }
            | Expr.Coerce(expr, ty) ->
                monad {
                    let! exprType = expr |> typing state

                    if isCompatible exprType ty then
                        return ty
                    else
                        return! Error(IncompatibleCoercion(exprType, ty))
                }
            | Expr.As(_, ty) -> Ok ty

        and typingBlock (state: TypingState) (block: Block) =
            let runStatement state statement =
                monad {
                    let! state = state
                    return! typingStatement state statement
                }

            monad {
                let! state' = (Ok state, block.Statements) ||> Seq.fold runStatement
                let! return' = block.Return |> Option.map (typing state') |> Option.defaultValue (Ok Type.Void)

                let returnType' =
                    state'.GlobalReturnTypes |> Set.add return' |> Seq.reduce (curry Type.Union)

                return returnType'
            }

        and typingStatement (state: TypingState) (statement: Statement) : Result<TypingState, TypingError> =
            match statement with
            | Let(Pattern.Variable(var, ty), expr) ->
                monad {
                    let! ty' = typing state expr

                    if ty.IsNone then
                        return TypingState.addVar var ty' Immutable state
                    elif isCompatible ty' ty.Value then
                        return TypingState.addVar var ty.Value Immutable state
                    else
                        return! Error(IncompatibleAssignment(ty.Value, ty'))
                }
            | Let(_, _) -> failwith "Not Implemented"
            | Var(Pattern.Variable(var, ty), expr) ->
                monad {
                    let! ty' = typing state expr |> Result.map widen

                    match ty with
                    | None -> return TypingState.addVar var ty' Mutable state
                    | Some ty ->
                        do! Result.assertWith (isCompatible ty' ty) (IncompatibleAssignment(ty, ty'))
                        return TypingState.addVar var ty Mutable state
                }
            | Var(_, _) -> failwith "Not Implemented"
            | Gets(Pattern.Variable(var, ty), expr) ->
                monad {
                    let! { Type = ty'; Mutability = mutability } = state.FindVar var
                    let! ty'' = typing state expr

                    do! Result.assertWith (mutability = Mutable) (ImmutableVariableReassigned var)
                    do! Result.assertWith (isCompatible ty'' ty') (IncompatibleAssignment(ty', ty''))

                    do!
                        Result.assertWith
                            (ty.IsNone || isCompatible ty'' ty.Value)
                            (IncompatibleAssignment(ty.Value, ty''))

                    return state
                }
            | Gets(_, _) -> failwith "Not Implemented"
            | Do expr -> typing state expr |> Result.map (fun _ -> state)
            | For(Pattern.Variable(var, ty), range, statements) ->
                monad {
                    let! rangeTy = typing state range

                    let isRangeTyCompatible = isCompatible rangeTy Type.Int
                    let isTyCompatible = ty.IsNone || isCompatible ty.Value Type.Int

                    do! Result.assertWith isRangeTyCompatible (InvalidForRange(rangeTy))
                    do! Result.assertWith isTyCompatible (IncompatibleAssignment(ty.Value, Type.Int))

                    let state' =
                        TypingState.addVar var (ty |> Option.defaultValue Type.Int) Immutable state // REMARK: Variables in for-loop are always immutable.

                    let! state'' =
                        (Ok state', statements)
                        ||> Seq.fold (fun state statement ->
                            state |> Result.bind (fun state -> typingStatement state statement))

                    return state''.ExitScopeInto state
                }
            | For(_, _, _) -> failwith "Not Implemented"
            | Return expr ->
                monad {
                    let! ty = typing state expr
                    return TypingState.addReturnType ty state
                }
            | RawExpr expr ->
                monad {
                    let! _ = typing state expr
                    return state
                }
