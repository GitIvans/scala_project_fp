package budgetapp.core

import budgetapp.domain._

object Analytics {

  def computeSummary(txs: List[Transaction]): Summary = {
    val (inc, exp) = txs.foldLeft((BigDecimal(0), BigDecimal(0))) {
      case ((i, e), tx) =>
        tx.tpe match {
          case TxType.Income  => (i + tx.amount, e)
          case TxType.Expense => (i, e + tx.amount)
        }
    }
    Summary(inc, exp)
  }

  def expensesByCategory(txs: List[Transaction]): List[(String, BigDecimal)] = {
    val pairs = txs.collect { case Transaction(_, TxType.Expense, cat, amt, _) => (cat, amt) }

    val totals: Map[String, BigDecimal] =
      pairs.groupBy(_._1).map { case (cat, xs) =>
        val sum = xs.foldLeft(BigDecimal(0)) { case (acc, (_, amt)) => acc + amt }
        cat -> sum
      }

    totals.toList.sortBy { case (_, sum) => -sum }
  }

  def topExpenses(txs: List[Transaction], n: Int): List[Transaction] =
    txs.filter(_.tpe == TxType.Expense).sortBy(tx => -tx.amount).take(n)
}
