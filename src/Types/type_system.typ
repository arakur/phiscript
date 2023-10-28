#set text(font: ("Constantia", "AR Ming B"))
#show strong: set text(font: ("Constantia", "Yu Gothic"))

#set heading(numbering: "1.1")

= Expressions

== Expressions

$
    e ::=
      & x space.quad (bold("variable")) \
    ‖ & λ space.quad (bold("literal")) \
    ‖ & [e_1, ..., e_n] \
    ‖ & {k_1: e_1 space ; space ... space ; space k_n: e_n} \
    ‖ & (x_1, ..., x_n) -> e \
    ‖ & e_1[e_2] \
    ‖ & f(e_1, ..., e_n) \
    ‖ & "if" e "elif" f_1 ... "elif" f_n "else" g "end" \
    ‖ & "match" e space ; space p_1 ⇒ e_1 space ; space ... space ; space p_n ⇒ e_n "end" \
    ‖ & (e: τ) \
    ‖ & e "as" τ
$

= Typing

== Types

#let int = $italic("int")$
#let number = $italic("number")$
#let string = $italic("string")$
#let bool = $italic("bool")$
#let null = $italic("null")$
#let void = $italic("void")$
#let any = $italic("any")$
#let some = $italic("some")$

$
    τ ::=
      & T space.quad (bold("type variable")) \
    ‖ & λ space.quad (bold("literal")) \
    ‖ & int ‖ number ‖ string ‖ bool ‖ null ‖ void \
    ‖ & [τ_1, ..., τ_n] ‖ τ[] \
    ‖ & {k_1: τ_1 space ; space ... space ; space k_n: τ_n} \
    ‖ & τ_1 ∣ τ_2 \
    ‖ & (τ_1, ..., τ_n) → σ \
    ‖ & any ‖ some
$

== Type Equivalence

$
    (τ₀ ∣ τ₁) ∣ τ₂ & ∼^c τ₀ ∣ (τ₁ ∣ τ₂) \
    τ₀ ∣ τ₁ & ∼^c τ₁ ∣ τ₀ \
    τ ∣ τ & ∼^c τ \
    any ∣ τ & ∼^c τ \
    some ∣ τ & ∼^c some
$

== Subtype Relation

$
    () / (τ ⪯ τ) \
    () / (any ⪯ τ) space.quad
    () / (τ ⪯ some) \
    ("typeof"(λ) ⪯ τ) / (λ ⪯ τ) \
    () / (int ⪯ number) \
    (τ_1 ⪯ σ_1 space.quad ... space.quad τ_n ⪯ σ_n) / ([τ_1, ..., τ_n] ⪯ [σ_1, ..., σ_n]) space.quad
    (τ_1 ⪯ σ space.quad ... space.quad τ_n ⪯ σ) / ([τ_1, ..., τ_n] ⪯ σ[]) space.quad
    (τ ⪯ σ) / (τ[] ⪯ σ[]) \
    (τ_1 ⪯ σ space.quad τ_2 ⪯ σ) / (τ_1 ∣ τ_2 ⪯ σ) space.quad
    (τ ⪯ σ_1) / (τ ⪯ σ_1 ∣ σ_2) \
    // (Mbar_1 ⪯ Mbar'_1) / ({Mbar_1; Mbar_2} ⪯ {Mbar'_1}) \
    (τ_1 ⪯ σ_1 space.quad ... space.quad τ_n ⪯ σ_n) / ({k_1: τ_1 space ; space ... space ; space k_n: τ_n space ; space k'_1: τ'_1 space ; space ... space ; space k'_m: τ'_m} ⪯ {k_1: σ_1 space ; space ... space ; space k_n: σ_n}) \
    (τ_2^1 ⪯ τ_1^1 space.quad ... space.quad τ_2^n ⪯ τ_1^n space.quad σ_1 ⪯ σ_2) / ((τ_1^1, ..., τ_1^n) → σ_1 ⪯ (τ_2^1, ..., τ_2^n) → σ_2)
$

== Expression Typing

$
    () / (Γ, x : T ⊢ x : T) \
    () / (Γ ⊢ λ: "typeof"(λ)) \
    (Γ ⊢ e_1: T_1 space.quad ... space.quad Γ ⊢ e_n: T_n) / (Γ ⊢ [e_1, ..., e_n]: [T_1, ..., T_n]) \
    (Γ ⊢ e: T) / (Γ ⊢ {k: e} : {k: T}) \
    (Γ, x_1: τ_1, ..., x_n: τ_n ⊢ e: σ) / (Γ ⊢ (x_1, ..., x_n) → e: (τ_1, ..., τ_n) → σ) \
    (Γ ⊢ e_1: [τ_1, ..., τ_n] space.quad Γ ⊢ e_2: i) / (Γ ⊢ e_1[e_2]: τ_i) \
    (Γ ⊢ e_1: [τ_1, ..., τ_n] space.quad Γ ⊢ e_2: null) / (Γ ⊢ e_1[e_2]: null) \
    (Γ ⊢ e_1: [τ_1, ..., τ_n] space.quad Γ ⊢ e_2: σ space.quad σ ⪯ int) / (Γ ⊢ e_1[e_2]: τ_1 ∣ ... ∣ τ_n ∣ null) \
    (Γ ⊢ f: (σ_1, ..., σ_n) → ρ space.quad Γ ⊢ e_1: τ_1 space.quad ... space.quad Γ ⊢ e_n: τ_n space.quad τ_1 ⪯ σ_1 space.quad ... space.quad τ_n ⪯ σ_n) / (Γ ⊢ f(e_1, ..., e_n): ρ) \
    (Γ ⊢ e: bool space.quad Γ ⊢ f_1: τ_1 space.quad ... space.quad Γ ⊢ f_n: τ_n space.quad Γ ⊢ g: τ')
    / (Γ ⊢ "if" e "elif" f_1 ... "elif" f_n "else" g "end": τ_1 ∣ ... ∣ τ_n ∣ τ') \
    (Γ ⊢ e: bool space.quad Γ ⊢ f_1: τ_1 space.quad ... space.quad Γ ⊢ f_n: τ_n)
    / (Γ ⊢ "match" e space ; space p_1 ⇒ f_1 space ; space ... space ; space p_n ⇒ f_n "end": τ_1 ∣ ... ∣ τ_n) \
    (Γ ⊢ e: τ space.quad Γ ⊢ τ ⪯ σ) / (Γ ⊢ (e: σ): σ) \
    () / (Γ ⊢ e "as" σ: σ)
$
