package uco.fp.banking.services

import java.time.LocalDate

import uco.fp.banking.domain._

object AccountOperations {

  import TransactionOps._

  def createAccount(entity: NewAccountRequest): Either[DomainError, Account[NewAccount]] = {
    for {
      ahnames <- AccountHolderName(first = entity.holderFirstName, last = entity.holderLastName)
      account <- Account(
        entity.accountName,
        number = entity.accountNumber,
        accountHolder = AccountHolder(
          id = entity.holderId,
          name = ahnames,
          personType = Natural
        ),
        address = AccountAddress(entity.holderAddress),
        `type` = AccountType(entity.accountType),
        balance = Amount(currency = USD, 0.0)
      )
    } yield account
  }

  def closeAccount(account: Account[Active]): Account[Closed] = {
    account.copy[Closed](closedDate = Some(LocalDate.now))
  }

  def initialDeposit[T: Transaction](amount: Amount, account: T): T =
    implicitly[Transaction[T]].credit(amount, account)


  def credit[T: Transaction](amount: Amount, account: T): T =
    implicitly[Transaction[T]].credit(amount, account)


  def debit[T: Transaction](amount: Amount, account: T): T =
    implicitly[Transaction[T]].debit(amount, account)

  def balance(account: Account[_]): Amount = account.balance

  def transfer[T: Transaction](amount: Amount, account1: T, account2: T): T =
    implicitly[Transaction[T]].transfer(account1, account2, amount)

  case class NewAccountRequest(
    accountName: String,
    accountNumber: Int,
    holderId: Int,
    holderFirstName: String,
    holderLastName: String,
    holderAddress: String,
    accountType: String
)

}
