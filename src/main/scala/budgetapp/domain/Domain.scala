package budgetapp.domain

import java.time.LocalDate

sealed trait TxType
object TxType {
  case object Income extends TxType
  case object Expense extends TxType

  def fromString(s: String): Either[AppError, TxType] =
    s.trim.toLowerCase match {
      case "income"  => Right(Income)
      case "expense" => Right(Expense)
      case other     => Left(ParseError(s"Unknown transaction type: '$other'"))
    }
}

final case class Transaction(
                              date: LocalDate,
                              tpe: TxType,
                              category: String,
                              amount: BigDecimal,
                              comment: String
                            )

final case class Summary(totalIncome: BigDecimal, totalExpense: BigDecimal) {
  def balance: BigDecimal = totalIncome - totalExpense
}

sealed trait Rule
object Rule {
  // Total expenses must be <= maxExpense
  final case class MonthlyExpenseLimit(maxExpense: BigDecimal) extends Rule
  // Category spend must be <= (income * percent / 100)
  final case class CategoryPercentLimit(category: String, maxPercentOfIncome: BigDecimal) extends Rule
}

final case class Warning(message: String)
