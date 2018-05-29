/// Provides access to the banking API.
module Capstone5.Api

open Capstone5.Domain
open Capstone5.Operations
open Capstone5.FileRepository
open System

let withdrawWithAudit amount (CreditAccount account as creditAccount) =
    auditAs "withdraw" Auditing.composedLogger withdraw amount creditAccount account.AccountId account.Owner

let depositWithAudit amount (ratedAccount:RatedAccount) =
    let accountId = ratedAccount.GetField(fun a -> a.AccountId)
    let owner = ratedAccount.GetField(fun a -> a.Owner)
    auditAs "deposit" Auditing.composedLogger deposit amount ratedAccount accountId owner

let internal getAccountFromName name =
    let transactions = tryFindTransactionsOnDisk name
    match transactions with
    | Some accountInfo -> loadAccount accountInfo
    | None -> loadAccount (name, Guid.NewGuid(), [])

/// Loads an account from disk. If no account exists, an empty one is automatically created.
let LoadAccount (customer:Customer) : RatedAccount =
    getAccountFromName customer.Name

/// Deposits funds into an account.
let Deposit (amount:decimal) (customer:Customer) : RatedAccount =
    getAccountFromName customer.Name
    |> depositWithAudit amount

/// Gets the balance from an account
let GetBalance account =
    match account with
    | Overdrawn account -> account.Balance
    | InCredit (CreditAccount account) -> account.Balance

/// Withdraws funds from an account that is in credit.
let Withdraw (amount:decimal) (customer:Customer) : RatedAccount =
    let account = getAccountFromName customer.Name
    match account with
    | InCredit (CreditAccount account as creditAccount) -> withdrawWithAudit amount creditAccount
    | Overdrawn _ -> account
                                 
/// Loads the transaction history for an owner. If no transactions exist, returns an empty sequence.
let LoadTransactionHistory(customer:Customer) : Transaction seq =
    match tryFindTransactionsOnDisk customer.Name with
    | None -> Seq.empty
    | Some (_,_,transactions) -> transactions

