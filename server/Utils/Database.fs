namespace TransportApplication.Utils

open System
open System.Linq
open System.Collections.Generic
open System.Data
open System.Data.Common
open Npgsql
open Dapper
open TransportApplication.Utils.Configuration

module Database =
    let inline (=>) a b = a, box b

    let connection : unit -> DbConnection =
        fun _ ->
            Dapper.DefaultTypeMap.MatchNamesWithUnderscores <- true
            new NpgsqlConnection(GlobalConnectionString) :> DbConnection

    module Transactions =
        module Internals =
            type Context = {
                conn : DbConnection
                tx : DbTransaction }

            type Result<'R> = Async<Result<'R, exn>>
            type Wrapper<'R> = Wrapper of (Context -> Result<'R>)

            let run<'R> (Wrapper f) (ctx : Context): Result<'R> =
                async {
                    try
                        return! f ctx
                    with
                    | ex -> return Error ex
                }

            type TransactionBuilder() =
                member this.ReturnFrom(m : Wrapper<'a>): Wrapper<'a> = m
                member this.Return(r : 'a): Wrapper<'a> =
                    Wrapper(fun _ -> async { return Ok r })

                member this.Bind(m : Wrapper<'a>, f : 'a -> Wrapper<'b>): Wrapper<'b> =
                    Wrapper(fun ctx ->
                        async {
                            let! rhs = run m ctx
                            match rhs with
                            | Ok res -> return! run (f res) ctx
                            | Error err -> return Error err
                        }
                    )

                member this.Zero(): Wrapper<'a> =
                    Wrapper(fun _ -> async { return Error (new InvalidOperationException("No database operation performed") :> exn) })

                member this.Combine(a : Wrapper<'a>, b : Wrapper<'b>): Wrapper<'a> = a

                member this.Delay(f : unit -> Wrapper<'a>): Wrapper<'a> = f()

                member this.Run(f : Wrapper<'a>): Result<'a> =
                    async {
                        let conn = connection()

                        do! conn.OpenAsync() |> Async.AwaitTask
                        let! tx = conn.BeginTransactionAsync(IsolationLevel.ReadCommitted).AsTask() |> Async.AwaitTask

                        let ctx = { conn = conn; tx = tx }

                        let! result = run f ctx

                        match result with
                        | Ok _ -> do! tx.CommitAsync() |> Async.AwaitTask
                        | Error _ -> do! tx.RollbackAsync() |> Async.AwaitTask

                        do! conn.DisposeAsync().AsTask() |> Async.AwaitTask
                        return result
                    }

        let query<'a> (sql : string) (param : IDictionary<string, obj>) =
            Internals.Wrapper(fun { conn = c; tx = tx } ->
                async {
                    let! result = c.QueryAsync<'a>(sql, param, transaction = tx) |> Async.AwaitTask
                    return Ok result
                }
            )

        let queryJoin<'a, 'b> (sql : string) (param : IDictionary<string, obj>) (foreignKey : string) (join : 'a -> 'b -> 'a) =
            Internals.Wrapper(fun { conn = c; tx = tx } ->
                async {
                    let! result = c.QueryAsync<'a, 'b, 'a>(sql, Func<'a, 'b, 'a>(join), param, transaction = tx, splitOn = foreignKey) |> Async.AwaitTask
                    return Ok result
                }
            )

        let execute (sql : string) (param : IDictionary<string, obj>) =
            Internals.Wrapper(fun { conn = c; tx = tx } ->
                async {
                    let! result = c.ExecuteAsync(sql, param, transaction = tx) |> Async.AwaitTask
                    return Ok result
                }
            )

        let transaction = new Internals.TransactionBuilder()
