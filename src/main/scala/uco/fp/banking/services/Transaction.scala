package uco.fp.banking.services

import uco.fp.banking.domain.{Account, Active, Amount, NewAccount, Closed}

trait Transaction[T] {
  def credit[A](a: A, b:T): T
  def debit[A](a: A, b:T): T
  def balance[A](b:T):A
  def transfer[A](a: T, b: T, c:A): T
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
    def balance[A <: Amount](b: Account[Active]): Amount = b.balance
    def transfer[A <: Amount](a: Account[Active], b: Account[Active], c: A): Account[Active] = {
      if(a.balance.value >= c.value) {
        credit(c,b)
        debit(c,a)
      }
      else
        a
    }
  }
  implicit object NewAccountTransactions extends Transaction[Account[NewAccount]] {
    def credit[A <: Amount](a: A, b: Account[Active]): Account[Active] = {
      val oldAmount = b.balance.value
      b.copy(balance = b.balance.copy(value = oldAmount + a.value))
    }
    def debit[A <: Amount](a: A, b: Account[Active]): Account[Active] = b
    def balance[A <: Amount](b: Account[Active]): Amount = b.balance
    def transfer[A <: Amount](a: Account[Active], b: Account[Active], c: A): Account[Active] = a
  }
  implicit object ClosedAccountTransactions extends Transaction[Account[Closed]] {
    def credit[A <: Amount](a: A, b: Account[Active]): Account[Active] = {
      a
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
    def balance[A <: Amount](b: Account[Active]): Amount = b.balance
    def transfer[A <: Amount](a: Account[Active], b: Account[Active], c: A): Account[Active] = {
      if(a.balance.value >= c.value) {
        credit(c,b)
        debit(c,a)
      }
      else
        a
    }
  }
}
