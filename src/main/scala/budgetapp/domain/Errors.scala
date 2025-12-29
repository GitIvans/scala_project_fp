package budgetapp.domain

sealed trait AppError {
  def message: String
}

final case class FileError(message: String) extends AppError
final case class ParseError(message: String) extends AppError
final case class CommandError(message: String) extends AppError
