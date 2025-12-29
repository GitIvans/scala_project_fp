package budgetapp.parser

import budgetapp.domain._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object CsvParser {

  // Expected header:
  // date,type,category,amount,comment
  // Example row:
  // 2025-12-01,expense,Food,12.50,coffee
  //
  // NOTE: comment must not contain commas in this simple parser.
  private val fmt = DateTimeFormatter.ISO_LOCAL_DATE

  def parseTransactions(lines: List[String]): Either[List[AppError], List[Transaction]] = {
    val dataLines = dropHeader(lines).filter(_.trim.nonEmpty)

    val parsed: List[Either[AppError, Transaction]] = dataLines.map(parseLine)

    val errors = parsed.collect { case Left(e) => e }
    if (errors.nonEmpty) Left(errors)
    else Right(parsed.collect { case Right(t) => t })
  }

  private def dropHeader(lines: List[String]): List[String] =
    if (lines.isEmpty) Nil else lines.drop(1)

  private def parseLine(line: String): Either[AppError, Transaction] = {
    val parts = line.split(",", -1).map(_.trim).toList
    parts match {
      case dateStr :: tpeStr :: category :: amountStr :: comment :: Nil =>
        for {
          date   <- parseDate(dateStr)
          tpe    <- TxType.fromString(tpeStr)
          amount <- parseAmount(amountStr)
          cat     = if (category.isEmpty) "Unknown" else category
        } yield Transaction(date, tpe, cat, amount, comment)

      case _ =>
        Left(ParseError(s"Bad CSV format: '$line'"))
    }
  }

  private def parseDate(s: String): Either[AppError, LocalDate] =
    try Right(LocalDate.parse(s, fmt))
    catch { case _: Throwable => Left(ParseError(s"Bad date: '$s' (expected yyyy-MM-dd)")) }

  private def parseAmount(s: String): Either[AppError, BigDecimal] =
    try Right(BigDecimal(s))
    catch { case _: Throwable => Left(ParseError(s"Bad amount: '$s'")) }
}
