namespace TransportApplication.Utils

open System
open System.Collections.Generic
open TransportApplication.Utils.Configuration

module Authentication =
    module Hashing =
        open System.Security.Cryptography
        open Microsoft.AspNetCore.Cryptography.KeyDerivation

        let getBytes(value: int): byte [] =
            if BitConverter.IsLittleEndian then
                BitConverter.GetBytes(value)
            else
                BitConverter.GetBytes(value) |> Array.rev

        let fromBytes(bytes: byte[]): int =
            let data =
                if BitConverter.IsLittleEndian then
                    bytes
                else
                    bytes |> Array.rev

            BitConverter.ToInt32(data, 0)

        let saltSize = 128 / 8
        let subkeySize = 256 / 8

        let resultSize =
            sizeof<int> + sizeof<int> + sizeof<KeyDerivationPrf> + saltSize + subkeySize

        let hashPassword (password : string): string =
            let iterations = 10000

            use rng = RandomNumberGenerator.Create()

            let salt = Array.zeroCreate<byte> saltSize
            rng.GetBytes(salt)

            let subkey =
                KeyDerivation.Pbkdf2(password, salt, KeyDerivationPrf.HMACSHA256, iterations, subkeySize)

            let result = Array.zeroCreate<byte> resultSize

            let version = getBytes(1)
            let prf = getBytes((int)KeyDerivationPrf.HMACSHA256)
            let iterations = getBytes(iterations)

            Buffer.BlockCopy(version, 0, result, 0, sizeof<int>)
            Buffer.BlockCopy(prf, 0, result, sizeof<int>, sizeof<int>)
            Buffer.BlockCopy(iterations, 0, result, sizeof<int>*2, sizeof<int>)
            Buffer.BlockCopy(salt, 0, result, sizeof<int>*3, saltSize)
            Buffer.BlockCopy(subkey, 0, result, sizeof<int>*3+saltSize, subkeySize)

            Convert.ToBase64String(result)

        let validateHash (hashedPassword : string) (input : string): bool =
            let bytes = Convert.FromBase64String(hashedPassword)
            if Array.length bytes <> resultSize then
                false
            else
                let versionBytes = Array.zeroCreate<byte> sizeof<int>
                let prfBytes = Array.zeroCreate<byte> sizeof<int>
                let iterationsBytes = Array.zeroCreate<byte> sizeof<int>

                let salt = Array.zeroCreate<byte> saltSize
                let subkey = Array.zeroCreate<byte> subkeySize

                Buffer.BlockCopy(bytes, 0, versionBytes, 0, sizeof<int>)
                Buffer.BlockCopy(bytes, sizeof<int>, prfBytes, 0, sizeof<int>)
                Buffer.BlockCopy(bytes, sizeof<int>*2, iterationsBytes, 0, sizeof<int>)
                Buffer.BlockCopy(bytes, sizeof<int>*3, salt, 0, saltSize)
                Buffer.BlockCopy(bytes, sizeof<int>*3+saltSize, subkey, 0, subkeySize)

                let version = fromBytes(versionBytes)
                if version <> 1 then
                    false
                else
                    let prf: KeyDerivationPrf = LanguagePrimitives.EnumOfValue <| fromBytes(prfBytes)
                    let iterations = fromBytes(iterationsBytes)

                    let inputSubkey = KeyDerivation.Pbkdf2(input, salt, prf, iterations, subkeySize)
                    Seq.compareWith Operators.compare subkey inputSubkey = 0

    module Jwt =
        open System.Security.Claims
        open System.IdentityModel.Tokens.Jwt
        open Microsoft.IdentityModel.Tokens

        let createJwtToken (conf : Settings) (claims : IDictionary<string, string>): string =
            let tokenClaims =
                claims
                |> Seq.map (|KeyValue|)
                |> Seq.map (fun (k,v) -> Claim(k, v))

            let expires = Nullable(DateTime.UtcNow.AddDays(1.0))
            let notBefore = Nullable(DateTime.UtcNow)
            let key = SymmetricSecurityKey(Text.Encoding.UTF8.GetBytes(conf.JwtSecret))
            let credentials = SigningCredentials(key = key, algorithm = SecurityAlgorithms.HmacSha256)

            let token =
                JwtSecurityToken(
                    issuer = conf.Issuer,
                    audience = conf.Audience,
                    claims = tokenClaims,
                    expires = expires,
                    notBefore = notBefore,
                    signingCredentials = credentials)

            JwtSecurityTokenHandler().WriteToken(token)

        let tokenValidationParams (conf : Settings): TokenValidationParameters =
            let key = SymmetricSecurityKey(Text.Encoding.UTF8.GetBytes(conf.JwtSecret))
            TokenValidationParameters(
                ValidateIssuerSigningKey = true,
                ValidateIssuer = true,
                ValidateAudience = true,
                IssuerSigningKey = key,
                ValidAudience = conf.Audience,
                ValidIssuer = conf.Issuer)

        let validateJwtToken (conf : Settings) (token : string) =
            let handler = JwtSecurityTokenHandler()
            handler.InboundClaimTypeMap.Clear()

            try
                let _, validatedToken =
                    handler.ValidateToken(token, tokenValidationParams conf)
                let claims = (validatedToken :?> JwtSecurityToken).Claims
                (true, claims)
            with _ -> (false, Seq.empty)

    module Http =
        open Giraffe
        open Microsoft.AspNetCore.Http
        open Microsoft.AspNetCore.Authentication.JwtBearer
        open System.Security.Claims

        let private notLoggedIn = RequestErrors.UNAUTHORIZED JwtBearerDefaults.AuthenticationScheme "TransportApplication" "User must be logged in."
        let mustBeLoggedIn (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            requiresAuthentication notLoggedIn next ctx

        let private accessDenied = setStatusCode 401 >=> text "Access Denied"
        let mustHaveRole (role : string) (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            (mustBeLoggedIn >=> authorizeUser
                (fun u -> u.HasClaim (role, true.ToString())) accessDenied) next ctx

        type HttpContext with
            member this.UserId() : int =
                let user = this.User
                if user <> null then
                    user.Claims
                    |> Seq.filter (fun c-> c.Type = "sub")
                    |> Seq.map (fun c -> c.Value)
                    |> Seq.head
                    |> int
                else
                    failwith "Getting user id on unathenticated request!"

    #if TESTS

    module Tests =
        open NUnit.Framework
        open FsUnit

        [<TestFixture>]
        module PasswordHashing =
            open Hashing
            [<Test>]
            let ``hashed password is different than given password`` () =
                hashPassword "test password"
                |> should not' (equal "test password")

            [<Test>]
            let ``hashed password can be validated`` () =
                let hashed = hashPassword "test password"
                validateHash hashed "test password"
                |> should equal true

            [<Test>]
            let ``two hashes of the same password should be different`` () =
                hashPassword "test"
                |> should not' (equal (hashPassword "test"))

            [<Test>]
            let ``invalid password should not pass`` () =
                let hashed = hashPassword "test password"
                validateHash hashed "invalid password"
                |> should equal false
    #endif
