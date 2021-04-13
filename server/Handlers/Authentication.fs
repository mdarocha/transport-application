namespace TransportApplication.Handlers

open System
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open TransportApplication.Entities.User
open TransportApplication.Database.UserRepository
open TransportApplication.Utils.Validation
open TransportApplication.Utils.Authentication.Jwt
open TransportApplication.Utils.Authentication.Hashing
open TransportApplication.Utils.Configuration

module Authentication =
    module Models =
        type ModelError =
            | InvalidEmail
            | InvalidPhone
            | MissingRequiredField
            | PasswordTooShort
            | UnknownUser
            | BadPassword
            | EmailExists

        type AuthResult = { token : string }

        [<CLIMutable>]
        type LoginArgs =
            {
                Email : string
                Password : string
            }
            interface IValidatable<ModelError> with
                member this.Validate() =
                    async {
                        let emptyEmail = String.IsNullOrEmpty this.Email
                        let emptyPassword = String.IsNullOrEmpty this.Password

                        if emptyEmail || emptyPassword then
                            return Some MissingRequiredField
                        elif not (validEmail this.Email) then
                            return Some InvalidEmail
                        else
                            return None
                    }

        [<CLIMutable>]
        type RegisterArgs =
            {
                Email : string
                Password : string
                PhoneNumber : string
                Name : string
                Surname : string
            }
            interface IValidatable<ModelError> with
                member this.Validate() =
                    async {
                        let emptyEmail = String.IsNullOrEmpty this.Email
                        let emptyPassword = String.IsNullOrEmpty this.Password
                        let emptyPhone = String.IsNullOrEmpty this.PhoneNumber
                        let emptyName = String.IsNullOrEmpty this.Name
                        let emptySurname = String.IsNullOrEmpty this.Surname

                        if emptyEmail || emptyPassword || emptyPhone || emptyName || emptySurname then
                            return Some MissingRequiredField
                        elif not (validEmail this.Email) then
                            return Some InvalidEmail
                        elif not (validPhone this.PhoneNumber) then
                            return Some InvalidPhone
                        elif String.length this.Password < 5 then
                            return Some PasswordTooShort
                        else
                            return None
                    }

    module Endpoints =
        open Models
        open System.Threading.Tasks

        let getToken (user : User) (conf : Settings) : AuthResult =
            let claims = dict [
                "sub", user.Id.ToString();
                "email", user.Email;
                "name", user.Name;
                "surname", user.Surname;
                "is_client", true.ToString();
                "is_driver", true.ToString();
                "is_owner", true.ToString();

            ]
            { token = createJwtToken conf claims }

        let register (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let! args = ctx.BindAndValidateJsonAsync<RegisterArgs, ModelError>()

                let createdUser (args : RegisterArgs) =
                    let password = hashPassword args.Password
                    createUser args.Name args.Surname args.PhoneNumber args.Email password

                let! response =
                    async {
                        match args with
                        | Ok args ->
                            let! user = createdUser args
                            return Successful.OK <| getToken user (ctx.GetSettings())
                        | Error err -> return RequestErrors.BAD_REQUEST err
                    }

                return! response next ctx
            }

        let login (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let! args = ctx.BindAndValidateJsonAsync<LoginArgs, ModelError>()

                let! user =
                    match args with
                    | Ok args -> getByEmail args.Email
                    | Error _ -> async { return None }

                let checkPassword (input : string) (user : User): bool =
                    validateHash user.PasswordHash input

                let response =
                    match args with
                    | Ok args ->
                        match user with
                        | Some user ->
                            match checkPassword args.Password user with
                            | true -> Successful.OK <| getToken user (ctx.GetSettings())
                            | false -> RequestErrors.BAD_REQUEST BadPassword

                        | None -> RequestErrors.BAD_REQUEST UnknownUser
                    | Error err -> RequestErrors.BAD_REQUEST err

                return! response next ctx
            }

    let handlers: HttpHandler =
        choose [
            POST >=> choose
                [ route "/register" >=> Endpoints.register
                  route "/login" >=> Endpoints.login
                ]
        ]
