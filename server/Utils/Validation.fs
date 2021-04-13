namespace TransportApplication.Utils

open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.ComponentModel.DataAnnotations

module Validation =
    type Errors<'E> =
        | InvalidJson
        | ValidationError of 'E

    type IValidatable<'E> =
        abstract member Validate: unit -> Async<'E option>

    type HttpContext with
        member this.BindAndValidateJsonAsync<'T, 'E when 'T :> IValidatable<'E>>() : Task<Result<'T, Errors<'E>>> =
            task {
                try
                    let! model = this.BindJsonAsync<'T>()
                    let! isValid = model.Validate()
                    return
                        match isValid with
                        | None -> Ok model
                        | Some err -> Error (ValidationError err)
                with
                | ex ->
                    printf "%A" ex
                    return Error InvalidJson
            }

        member this.TryBindJsonAsync<'T>() : Task<Result<'T, exn>> =
            task {
                try
                    let! res = this.BindJsonAsync<'T>()
                    return Ok res
                with
                | ex ->
                    return Error ex
            }

    let validEmail (email : string) =
        let attr = new EmailAddressAttribute()
        attr.IsValid(email)

    let validPhone (phone : string) =
        true //TODO
