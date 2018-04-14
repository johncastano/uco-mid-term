package uco.fp.banking.domain

import java.time.LocalDate

sealed trait PersonType
case object Juristic extends PersonType
case object Natural extends PersonType

case class AccountHolderName(first: String, last:String)
object AccountHolderName {
  def apply(first: String, last: String): Either[DomainError, AccountHolderName] = {
    for {
      firstName <- validateNames(first)
      lastName <- validateNames(last)
    } yield new AccountHolderName(firstName, lastName)
  }
  private[this] def validateNames(field: String): Either[DomainError, String] =
    if(field.matches("^[a-zA-Z]+$"))
      Right(field)
    else Left(InvalidFirstName())
}

case class AccountHolder(
  id: Int,
  name: AccountHolderName,
  personType: PersonType
)

sealed trait AccountType
case object Checks extends AccountType
case object Saving extends AccountType
case object Fiduciary extends AccountType

object AccountType {
  def apply(value:String): AccountType = {
    value.toLowerCase match {
      case "checks"    => Checks
      case "saving"    => Saving
      case "fiduciary" => Fiduciary
    }
  }
}

case class AccountAddress(value: String)

sealed trait AccountState
trait NewAccount extends AccountState
trait Active extends AccountState
trait Closed extends AccountState

case class Amount(currency: Currency, value: Double)

case class Account[S <: AccountState](
  name: String,
  number: Int,
  accountHolder: AccountHolder,
  address: AccountAddress,
  openDate: LocalDate,
  closedDate: Option[LocalDate] = None,
  `type`: AccountType,
  balance: Amount
)
object Account {
  def apply(
  name: String,
  number: Int,
  accountHolder: AccountHolder,
  address: AccountAddress,
  `type`: AccountType,
  balance: Amount,
): Either[DomainError, Account[NewAccount]] = {
    for {
      accountName <- validateName(name)
    } yield new Account(accountName, number, accountHolder, address, LocalDate.now, None, `type`, balance)
  }
  private[this] def validateName(field: String): Either[DomainError, String] = {
    if(field.matches("^[a-zA-Z]+$"))
      Right(field)
    else Left(InvalidAccountName())
  }
}



