package uco.fp.banking.domain

sealed trait Currency
case object USD extends Currency
case object COP extends Currency

