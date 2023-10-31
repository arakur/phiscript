namespace Utils

open FSharpPlus

module Result =
    let sequence (result: Result<'T, 'Error> seq) =
        result
        |> Seq.fold
            (fun acc result ->
                monad {
                    let! acc = acc
                    let! result = result
                    return result :: acc
                })
            (Ok [])
        |> Result.map Seq.rev

    let assertWith (condition: bool) (error: 'Error) = if condition then Ok() else Error error
