namespace TransportApplication.Utils

module Misc =
    let okOrRaise<'a> (result : Async<Result<'a, exn>>): Async<'a> =
        async {
            let! result = result
            return
                match result with
                | Ok r -> r
                | Error err -> raise err
        }

    let ignoreOrRaise<'a> (result : Async<Result<'a, exn>>): Async<unit> =
        async {
            let! result = result
            return
                match result with
                | Ok _ -> ()
                | Error err -> raise err
        }

    let stringToBool (input : string): bool =
        match System.Boolean.TryParse(input) with
        | (true, b) -> b
        | _ -> false
