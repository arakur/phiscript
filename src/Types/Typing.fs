namespace Types

open FSharpPlus

open Utils

type TypingError =
    | UndefinedVariable of Syntax.Var
    | UndefinedTypeVariable of Syntax.Var
    | UndefinedUnOp of Syntax.UnOp
    | UndefinedBinOp of Syntax.BinOp
    | UnOpArgumentTypeMismatch of op: Syntax.UnOp * expected: Type list * actual: Type
    | BinOpArgumentTypeMismatch of op: Syntax.BinOp * expected: (Type * Type) list * actual: (Type * Type)
    | IncompatibleAssignment of source: Type * target: Type
    | ImmutableVariableReassigned of var: Syntax.Var
    | NotAFunction of Type
    | ArgumentNumberMismatch of expected: int * actual: int
    | ArgumentTypeMismatch of index: int * expected: Type * actual: Type
    | CannotIndexWith of index: Type * target: Type
    | CannotAccessWith of key: Syntax.Key * target: Type
    | IfConditionTypeNotCompatibleWithBoolean of Type
    | InvalidForRange of Type
    | IncompatiblePattern of pattern: Syntax.Pattern * target: Type
    | IncompatibleCoercion of source: Type * target: Type

type Typing = Result<Type, TypingError>

module Typing =
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

    let rec widen (ty: Type) =
        match ty with
        | Type.Literal lit -> Literal.typeOf lit
        | Type.SizedArray tys -> Type.SizedArray(tys |> List.map widen)
        | Type.Array ty -> Type.Array(widen ty)
        | Type.Object obj -> Type.Object(obj |> Map.map (fun _key -> widen))
        | Type.Union(lhs, rhs) ->
            let lhs' = widen lhs
            let rhs' = widen rhs

            if isCompatible lhs' rhs' then rhs'
            elif isCompatible rhs' lhs' then lhs'
            else Type.Union(lhs', rhs')
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
        { Variables: Map<Syntax.Var, VarType>
          TVariables: Map<Syntax.Var, Type>
          UnOps: Map<Syntax.UnOp, (Type * Type) list>
          BinOps: Map<Syntax.BinOp, (Type * Type * Type) list>
          GlobalReturnTypes: Type Set }

        static member empty =
            { Variables = Map.empty
              TVariables = Map.empty
              UnOps = Map.empty
              BinOps = Map.empty
              GlobalReturnTypes = Set.empty }

        static member addVar (var: Syntax.Var) (ty: Type) (mutability: Mutability) (this: TypingState) =
            { this with
                Variables = this.Variables |> Map.add var { Type = ty; Mutability = mutability } }

        member this.FindVar var =
            this.Variables.TryFind var |> Option.toResultWith (UndefinedVariable var)

        static member addTVar (var: Syntax.Var) (ty: Type) (this: TypingState) =
            { this with
                TVariables = this.TVariables |> Map.add var ty }

        member this.FindTVar var =
            this.TVariables.TryFind var |> Option.toResultWith (UndefinedVariable var)

        static member addUnOp (op: Syntax.UnOp) (arg: Type) (ret: Type) (this: TypingState) =
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

        static member addBinOp (op: Syntax.BinOp) (lhs: Type) (rhs: Type) (ret: Type) (this: TypingState) =
            let types: (Type * Type * Type) list =
                this.BinOps |> Map.tryFind op |> Option.defaultValue []

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
        let typing (numeral: Syntax.Numeral) =
            LiteralType.Numeral numeral |> Type.Literal |> Ok

        let widen (numeral: Syntax.Numeral) =
            match numeral.Decimal with
            | Some _ -> Type.Number |> Ok
            | None -> Type.Int |> Ok

    module Expr =
        let rec tryTypeFromSyntax (state: TypingState) (synTy: Syntax.Type) : Result<Type, TypingError> =
            match synTy with
            | Syntax.Type.TVar var -> state.FindTVar var
            | Syntax.Type.Any -> Ok Type.Any
            | Syntax.Type.Some -> Ok Type.Some
            | Syntax.Type.Literal lit -> Ok(Type.Literal <| LiteralType.fromSyntax lit)
            | Syntax.Type.Int -> Ok Type.Int
            | Syntax.Type.Number -> Ok Type.Number
            | Syntax.Type.String -> Ok Type.String
            | Syntax.Type.Bool -> Ok Type.Bool
            | Syntax.Type.Null -> Ok Type.Null
            | Syntax.Type.Array ty -> ty |> tryTypeFromSyntax state |> Result.map Type.Array
            | Syntax.Type.SizedArray tys ->
                tys
                |> List.map (tryTypeFromSyntax state >> Result.map widen)
                |> Result.sequence
                |> Result.map List.ofSeq
                |> Result.map Type.SizedArray
            | Syntax.Type.Object tys ->
                tys
                |> Map.toSeq
                |> Seq.map (fun (key, ty) -> ty |> tryTypeFromSyntax state |> Result.map (fun ty -> key, ty))
                |> Result.sequence
                |> Result.map Map.ofSeq
                |> Result.map Type.Object
            | Syntax.Type.Function(args, ret) ->
                monad {
                    let! args' =
                        args
                        |> List.map (tryTypeFromSyntax state >> Result.map widen)
                        |> Result.sequence
                        |> Result.map List.ofSeq

                    let! ret' = ret |> tryTypeFromSyntax state |> Result.map widen
                    return Type.Function(args', ret')
                }
            | Syntax.Type.Union(lhs, rhs) ->
                monad {
                    let! lhs' = lhs |> tryTypeFromSyntax state |> Result.map widen
                    let! rhs' = rhs |> tryTypeFromSyntax state |> Result.map widen
                    return Type.Union(lhs', rhs')
                }
            | Syntax.Type.TypeOf expr -> expr |> typing state

        and typingPattern (state: TypingState) (pattern: Syntax.Pattern) =
            match pattern with
            | Syntax.Pattern.Numeral numeral -> numeral |> Numeral.typing
            | Syntax.Pattern.StringLit _ -> Ok Type.String
            | Syntax.Pattern.Variable(_, synTy) ->
                synTy
                |> Option.map (tryTypeFromSyntax state)
                |> Option.defaultValue (Ok Type.Any)
            | Syntax.Pattern.Array array ->
                array
                |> List.map (typingPattern state)
                |> Result.sequence
                |> Result.map List.ofSeq
                |> Result.map Type.SizedArray
            | Syntax.Pattern.Wildcard -> Ok Type.Any

        and typing (state: TypingState) (expr: Syntax.Expr) =
            match expr with
            // ==============
            //  Γ, x:τ ⊢ x:τ
            | Syntax.Expr.Variable var -> state.FindVar var |> Result.map VarType.type_
            // ==================
            //  Γ ⊢ λ: typeOf(λ)
            | Syntax.Expr.Numeral numeral -> numeral |> Numeral.typing
            | Syntax.Expr.StringLit stringLit -> Ok(Type.Literal <| LiteralType.StringLit stringLit)
            //  Γ ⊢ e₁: τ₁ ... Γ ⊢ eₙ: τₙ
            // ====================================================
            //  Γ ⊢ { k₁: e₁; ...; kₙ: eₙ }: { k₁: τ₁; ...; kₙ: τₙ }
            | Syntax.Expr.Object obj ->
                obj
                |> List.map (fun (key, value) -> value |> typing state |> Result.map (fun ty -> key, ty))
                |> Result.sequence
                |> Result.map Map.ofSeq
                |> Result.map Type.Object
            //  Γ, x₁: τ₁, ..., xₙ: τₙ ⊢ e: ρ
            // ===================================================
            //  Γ ⊢ (x₁: τ₁, ..., xₙ: τₙ) -> e: (τ₁, ..., τₙ) -> ρ
            | Syntax.Expr.Lambda(args, body) ->
                monad {
                    let! (argTypes: Map<Syntax.Var, VarType>) =
                        args
                        |> List.map (function
                            | Syntax.Pattern.Variable(var, synTy) ->
                                let ty =
                                    synTy
                                    |> Option.map (tryTypeFromSyntax state >> Result.map widen)
                                    |> Option.defaultValue (Ok Type.Some)
                                // TODO: Implement type inference to infer type of variable enough precisely.
                                ty |> Result.map (fun ty -> Some(var, ty))
                            | Syntax.Pattern.Wildcard -> Ok None
                            | _ -> failwith "Not Implemented")
                        |> Result.sequence
                        |> Result.map (Seq.choose id)
                        |> Result.map (Seq.map (fun (var, ty) -> var, { Type = ty; Mutability = Immutable })) // REMARK: Arguments of lambda are always immutable.
                        |> Result.map Map.ofSeq

                    let state' =
                        { state with
                            Variables = state.Variables |> Map.union argTypes }

                    let! retType = body |> typing state'

                    return
                        Type.Function(
                            argTypes |> Map.toSeq |> Seq.map snd |> Seq.map VarType.type_ |> List.ofSeq,
                            retType
                        )
                }
            | Syntax.Expr.Array array ->
                array
                |> List.map (typing state)
                |> Result.sequence
                |> Result.map List.ofSeq
                |> Result.map Type.SizedArray
            | Syntax.Expr.IndexAccess(expr, index) ->
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
                    | Type.Object obj, Type.Literal(LiteralType.StringLit(Syntax.StringLit content)) ->
                        return
                            obj
                            |> Map.tryFind (Syntax.Key.Name <| Syntax.VarName.mk content)
                            |> Option.defaultValue Type.Some
                    | Type.Object _, _ when isCompatible indexType Type.String -> return Type.Some
                    | Type.Object _, _ -> return! Error(CannotIndexWith(indexType, exprType))
                    | _ -> return! Error(CannotIndexWith(indexType, exprType))
                }
            | Syntax.Expr.FieldAccess(expr, key) ->
                expr
                |> typing state
                |> Result.bind (fun exprTy ->
                    match exprTy with
                    | Type.Object tys -> tys |> Map.tryFind key |> Option.defaultValue Type.Some |> Ok
                    | Type.SizedArray tys ->
                        match key with
                        | Syntax.Key.Index index ->
                            tys
                            |> List.tryItem index
                            |> Option.toResultWith (CannotIndexWith(Type.Int, Type.SizedArray tys))
                        | _ -> Error(CannotAccessWith(key, exprTy))
                    | Type.Array ty ->
                        match key with
                        | Syntax.Key.Index _ -> Ok ty
                        | _ -> Error(CannotAccessWith(key, exprTy))
                    | Type.Any -> Ok Type.Any
                    | _ -> Error(CannotAccessWith(key, exprTy)))
            | Syntax.Expr.UnOp(_) -> failwith "Not Implemented"
            | Syntax.Expr.UnOpApplied(op, arg) ->
                monad {
                    let! argType = arg |> typing state

                    let! retType = state.FindUnOp op argType

                    return retType
                }
            | Syntax.Expr.BinOp(_) -> failwith "Not Implemented"
            | Syntax.Expr.BinOpApplied(op, lhs, rhs) ->
                monad {
                    let! lhsTy = lhs |> typing state
                    let! rhsTy = rhs |> typing state

                    let! retTy = state.FindBinOp op lhsTy rhsTy

                    return retTy
                }
            | Syntax.Expr.Apply(fun_, args) ->
                monad {
                    let! funType = fun_ |> typing state

                    match funType with
                    | Type.Function(expectedArgTypes, retType) ->
                        let! argTypes = args |> List.map (typing state) |> Result.sequence

                        do!
                            Result.assertWith
                                (List.length expectedArgTypes = Seq.length argTypes)
                                (ArgumentNumberMismatch(List.length expectedArgTypes, Seq.length argTypes))

                        for index, (argTy, expectedTy) in Seq.zip argTypes expectedArgTypes |> Seq.indexed do
                            do!
                                Result.assertWith
                                    (isCompatible argTy expectedTy)
                                    (ArgumentTypeMismatch(index, expectedTy, argTy))

                        return retType
                    | Type.Any -> return Type.Any
                    | _ -> return! Error(NotAFunction funType)
                }
            | Syntax.Expr.EvalBlock block -> typingBlock state block
            | Syntax.Expr.If(cases, else_) ->
                let caseType (cond: Syntax.Expr, block: Syntax.Block) : Result<Type, TypingError> =
                    cond
                    |> typing state
                    |> Result.bind (fun ty ->
                        if isCompatible ty Type.Bool then
                            Ok block
                        else
                            Error(IfConditionTypeNotCompatibleWithBoolean ty))
                    |> Result.bind (typingBlock state)

                monad {
                    let! cases' = cases |> List.map caseType |> Result.sequence |> Result.map List.ofSeq

                    let! cases'' =
                        match else_ with
                        | None -> Ok cases'
                        | Some else_ -> typingBlock state else_ |> Result.map (fun else_ -> else_ :: cases')

                    let retTy = cases' |> Seq.reduce (fun lhs rhs -> Type.Union(lhs, rhs))

                    return retTy
                }
            | Syntax.Expr.Match(expr, cases) ->
                let caseType (exprTy: Type) (pat: Syntax.Pattern, block: Syntax.Block) : Result<Type, TypingError> =
                    monad {
                        let! patTy = pat |> typingPattern state
                        let! blockTy = typingBlock state block
                        do! Result.assertWith (isCompatible patTy exprTy) (IncompatiblePattern(pat, exprTy))
                        return blockTy
                    }

                monad {
                    let! ty = expr |> typing state
                    let! cases' = cases |> List.map (caseType ty) |> Result.sequence
                    return cases' |> Seq.reduce (fun lhs rhs -> Type.Union(lhs, rhs))
                }
            | Syntax.Expr.Coerce(expr, synTy) ->
                monad {
                    let! exprType = expr |> typing state
                    let! ty = synTy |> tryTypeFromSyntax state

                    if isCompatible exprType ty then
                        return ty
                    else
                        return! Error(IncompatibleCoercion(exprType, ty))
                }
            | Syntax.Expr.As(_, synTy) -> synTy |> tryTypeFromSyntax state

        and typingBlock (state: TypingState) (block: Syntax.Block) =
            let runStatement state statement =
                monad {
                    let! state = state
                    return! typingStatement state statement
                }

            monad {
                let! state' = (Ok state, block.Statements) ||> Seq.fold runStatement
                let! return' = block.Return |> Option.map (typing state') |> Option.defaultValue (Ok Type.Null)

                let returnType' =
                    state'.GlobalReturnTypes |> Set.add return' |> Seq.reduce (curry Type.Union)

                return returnType'
            }

        and typingStatement (state: TypingState) (statement: Syntax.Statement) : Result<TypingState, TypingError> =
            match statement with
            | Syntax.Statement.Let(Syntax.Pattern.Variable(var, synTy), expr) ->
                monad {
                    let! ty =
                        match synTy with
                        | Some synTy -> synTy |> tryTypeFromSyntax state |>> Some
                        | None -> Ok None

                    let! ty' = typing state expr

                    if ty.IsSome then
                        let ty = ty.Value
                        do! Result.assertWith (isCompatible ty' ty) (IncompatibleAssignment(ty, ty'))

                    return TypingState.addVar var (ty |> Option.defaultValue ty') Immutable state
                }
            | Syntax.Statement.Let(_, _) -> failwith "Not Implemented"
            | Syntax.Statement.Var(Syntax.Pattern.Variable(var, synTy), expr) ->
                monad {
                    let! ty =
                        match synTy with
                        | Some synTy -> synTy |> tryTypeFromSyntax state |>> Some
                        | None -> Ok None

                    let! ty' = typing state expr |> Result.map widen

                    match ty with
                    | None -> return TypingState.addVar var ty' Mutable state
                    | Some ty ->
                        do! Result.assertWith (isCompatible ty' ty) (IncompatibleAssignment(ty, ty'))
                        return TypingState.addVar var ty Mutable state
                }
            | Syntax.Statement.Var(_, _) -> failwith "Not Implemented"
            | Syntax.Statement.Gets(Syntax.Pattern.Variable(var, synTy), expr) ->
                monad {
                    let! ty =
                        match synTy with
                        | Some synTy -> synTy |> tryTypeFromSyntax state |>> Some
                        | None -> Ok None

                    let! { Type = ty'; Mutability = mutability } = state.FindVar var
                    let! ty'' = typing state expr

                    do! Result.assertWith (mutability = Mutable) (ImmutableVariableReassigned var)
                    do! Result.assertWith (isCompatible ty'' ty') (IncompatibleAssignment(ty', ty''))

                    if ty.IsSome then
                        let ty = ty.Value
                        do! Result.assertWith (isCompatible ty'' ty) (IncompatibleAssignment(ty, ty''))

                    return state
                }
            | Syntax.Statement.Gets(_, _) -> failwith "Not Implemented"
            | Syntax.Statement.Do expr -> typing state expr |> Result.map (fun _ -> state)
            | Syntax.Statement.For(Syntax.Pattern.Variable(var, synTy), range, statements) ->
                monad {
                    let! ty =
                        match synTy with
                        | Some synTy -> synTy |> tryTypeFromSyntax state |>> Some
                        | None -> Ok None

                    let! rangeTy = typing state range

                    do! Result.assertWith (isCompatible rangeTy Type.Int) (InvalidForRange(rangeTy))

                    if ty.IsSome then
                        let ty = ty.Value
                        do! Result.assertWith (isCompatible ty Type.Int) (IncompatibleAssignment(ty, Type.Int))

                    let state' =
                        TypingState.addVar var (ty |> Option.defaultValue Type.Int) Immutable state // REMARK: Variables in for-loop are always immutable.

                    let! state'' =
                        (Ok state', statements)
                        ||> Seq.fold (fun state statement ->
                            state |> Result.bind (fun state -> typingStatement state statement))

                    return state''.ExitScopeInto state
                }
            | Syntax.Statement.For(_, _, _) -> failwith "Not Implemented"
            | Syntax.Statement.Return expr ->
                monad {
                    let! ty = expr |> Option.map (typing state) |> Option.defaultValue (Ok Type.Null)
                    return TypingState.addReturnType ty state
                }
            | Syntax.Statement.Break
            | Syntax.Statement.Continue -> Ok state
            | Syntax.Statement.RawExpr expr ->
                monad {
                    let! _ = typing state expr
                    return state
                }
            | Syntax.Statement.TypeDecl(name, synTy) ->
                monad {
                    let! ty = synTy |> tryTypeFromSyntax state
                    return TypingState.addTVar name ty state
                }
