package budgetapp.cli

import budgetapp.domain._

object Cli {

  sealed trait Command
  final case class SummaryCmd(file: String) extends Command
  final case class ByCategoryCmd(file: String) extends Command
  final case class TopCmd(file: String, n: Int) extends Command
  final case class CheckCmd(file: String) extends Command
  case object HelpCmd extends Command

  def parseArgs(args: List[String]): Either[AppError, Command] = args match {
    case "summary" :: file :: Nil        => Right(SummaryCmd(file))
    case "by-category" :: file :: Nil    => Right(ByCategoryCmd(file))
    case "top" :: file :: nStr :: Nil    => parseInt(nStr).map(n => TopCmd(file, n))
    case "check" :: file :: Nil          => Right(CheckCmd(file))
    case "help" :: Nil                   => Right(HelpCmd)
    case Nil                             => Right(HelpCmd)
    case _                               => Left(CommandError("Bad arguments. Use 'help' to see usage."))
  }

  def usage: String =
    """Usage:
      |  summary <csvFile>
      |  by-category <csvFile>
      |  top <csvFile> <N>
      |  check <csvFile>
      |
      |CSV format:
      |  date,type,category,amount,comment
      |Example:
      |  2025-12-01,expense,Food,12.50,coffee
      |""".stripMargin

  private def parseInt(s: String): Either[AppError, Int] =
    try Right(s.toInt)
    catch { case _: Throwable => Left(CommandError(s"Bad number: '$s'")) }
}
