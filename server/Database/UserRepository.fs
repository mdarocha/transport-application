namespace TransportApplication.Database

open System
open System.Data.Common
open TransportApplication.Utils.Misc
open TransportApplication.Utils.Database
open TransportApplication.Utils.Database.Transactions
open TransportApplication.Entities.User

module UserRepository =
    let getByEmail (email : string): Async<User option> =
        let sql = """
            select * from transport.user
            where email = :email
        """
        let data = dict [ "email" => email ]

        transaction {
            let! users = query<User> sql data
            return users |> Seq.tryHead
        } |> okOrRaise

    let createUser (name : string) (surname : string) (phone : string) (email : string) (passwordHash : string): Async<User> =
        transaction {
            let userData = dict [
                "name" => name
                "surname" => surname
                "phone_number" => phone
                "email" => email
                "password_hash" => passwordHash
            ]
            let userSql =
                """
                    insert into transport.user
                        (name, surname, phone_number, email, password_hash)
                    values
                        (:name, :surname, :phone_number, :email, :password_hash)
                    returning *
                """

            let! createdUser = query<User> userSql userData

            let createdUser =
                match createdUser |> Seq.tryHead with
                | Some user -> user
                | None -> failwith "No user created!"
            return createdUser
        } |> okOrRaise
