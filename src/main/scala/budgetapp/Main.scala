package budgetapp

import budgetapp.cli.Cli
import budgetapp.core.{Analytics, RulesEngine}
import budgetapp.domain._
import budgetapp.io.FileIO
import budgetapp.parser.CsvParser

object Main extends App {

  Cli.parseArgs(args.toList) match {
    case Left(err) =>
      println("ERROR: " + err.message)
      println(Cli.usage)

    case Right(Cli.HelpCmd) =>
      println(Cli.usage)

    case Right(cmd) =>
      run(cmd)
  }

  private def run(cmd: Cli.Command): Unit = cmd match {
    case Cli.SummaryCmd(file) =>
      load(file) match {
        case Left(errs) => printErrors(errs)
        case Right(txs) =>
          val s = Analytics.computeSummary(txs)
          println(s"Income:  ${s.totalIncome}")
          println(s"Expense: ${s.totalExpense}")
          println(s"Balance: ${s.balance}")
      }

    case Cli.ByCategoryCmd(file) =>
      load(file) match {
        case Left(errs) => printErrors(errs)
        case Right(txs) =>
          Analytics.expensesByCategory(txs).foreach { case (cat, total) =>
            println(f"$cat%-15s $total")
          }
      }

    case Cli.TopCmd(file, n) =>
      load(file) match {
        case Left(errs) => printErrors(errs)
        case Right(txs) =>
          Analytics.topExpenses(txs, n).foreach { t =>
            println(s"${t.date} | ${t.category} | ${t.amount} | ${t.comment}")
          }
      }

    case Cli.CheckCmd(file) =>
      // Simple default rules (keep it simple but “unique”)
      val rules: List[Rule] = List(
        Rule.MonthlyExpenseLimit(500),
        Rule.CategoryPercentLimit("Food", 30)
      )

      load(file) match {
        case Left(errs) => printErrors(errs)
        case Right(txs) =>
          val warnings = RulesEngine.applyRules(txs, rules)
          if (warnings.isEmpty) println("No warnings")
          else warnings.foreach(w => println("WARNING: " + w.message))
      }

    case Cli.HelpCmd =>
      println(Cli.usage)
  }

  private def load(file: String): Either[List[AppError], List[Transaction]] = {
    FileIO.readAllLines(file) match {
      case Left(err) => Left(List(err))
      case Right(lines) =>
        CsvParser.parseTransactions(lines)
    }
  }

  private def printErrors(errs: List[AppError]): Unit = {
    println("ERRORS:")
    errs.foreach(e => println("  - " + e.message))
  }
}
