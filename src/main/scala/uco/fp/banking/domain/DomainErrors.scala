package uco.fp.banking.domain

trait DomainError {
  val code: Int
  val message: String
}

final case class InvalidFirstName(code: Int = 1000, message: String = "Invalid First Name")      extends DomainError
final case class InvalidLastName(code: Int = 1100, message: String = "Invalid Last Name")        extends DomainError
final case class InvalidAccountName(code: Int = 10100, message: String = "Invalid Account Name") extends DomainError
