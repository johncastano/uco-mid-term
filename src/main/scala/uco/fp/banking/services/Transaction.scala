package uco.fp.banking.services

import uco.fp.banking.domain.{ Account, Active, Amount, NewAccount, Closed }

trait Transaction[T] {
  def credit[A <: Amount](a: A, b: T): T
  def debit[A <: Amount](a: A, b: T): T
  def transfer[A <: Amount](a: T, b: T, c: A): T
}

object TransactionOps {
  implicit object ActiveAccountTransactions extends Transaction[Account[Active]] {
    def credit[A <: Amount](a: A, b: Account[Active]): Account[Active] = {
      val oldAmount = b.balance.value
      b.copy(balance = b.balance.copy(value = oldAmount + a.value))
    }
    def debit[A <: Amount](a: A, b: Account[Active]): Account[Active] = {
      val oldAmount = b.balance.value
      val newAmount = b.balance.copy(value = oldAmount - a.value)
      val finalAmount = {
        if (newAmount.value < 0)
          newAmount.copy(value = 0)
        else
          newAmount
      }
      b.copy(balance = finalAmount)
    }
    def transfer[A <: Amount](a: Account[Active], b: Account[Active], c: A): Account[Active] = {
      if (a.balance.value >= c.value) {
        credit(c, b)
        debit(c, a)
      } else
        a
    }
  }

  implicit object NewAccountTransactions extends Transaction[Account[NewAccount]] {
    def credit[A <: Amount](a: A, b: Account[NewAccount]): Account[NewAccount] = {
      val oldAmount = b.balance.value
      b.copy(balance = b.balance.copy(value = oldAmount + a.value))
    }
    def debit[A](a: A, b: Account[NewAccount]): Account[NewAccount]                            = b
    def transfer[A](a: Account[NewAccount], b: Account[NewAccount], c: A): Account[NewAccount] = a
  }

  implicit object ClosedAccountTransactions extends Transaction[Account[Closed]] {
    def credit[A](a: A, b: Account[Closed]): Account[Closed]                                 = b
    def debit[A](a: A, b: Account[Closed]): Account[Closed]                                  = b
    def transfer[A <: Amount](a: Account[Closed], b: Account[Closed], c: A): Account[Closed] = b
  }
}
