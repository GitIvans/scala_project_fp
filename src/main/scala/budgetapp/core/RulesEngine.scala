package budgetapp.core

import budgetapp.domain._

object RulesEngine {

  def applyRules(txs: List[Transaction], rules: List[Rule]): List[Warning] = {
    val summary = Analytics.computeSummary(txs)
    val byCat = Analytics.expensesByCategory(txs).toMap

    rules.flatMap {
      case Rule.MonthlyExpenseLimit(maxExpense) =>
        if (summary.totalExpense > maxExpense)
          List(Warning(s"Total expenses ${summary.totalExpense} exceed limit $maxExpense"))
        else Nil

      case Rule.CategoryPercentLimit(category, maxPercent) =>
        val spent   = byCat.getOrElse(category, BigDecimal(0))
        val income  = summary.totalIncome
        val allowed = if (income <= 0) BigDecimal(0) else (income * maxPercent) / 100

        if (income > 0 && spent > allowed)
          List(Warning(s"Category '$category' spent $spent exceeds $maxPercent% of income (allowed $allowed)"))
        else Nil
    }
  }
}
