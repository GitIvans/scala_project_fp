package budgetapp.io

import budgetapp.domain.{AppError, FileError}
import budgetapp.util.Using

import scala.io.Source

object FileIO {

  def readAllLines(path: String): Either[AppError, List[String]] = {
    Using(Source.fromFile(path)) { src =>
      src.getLines().toList
    } match {
      case Left(t)      => Left(FileError(t.getMessage))
      case Right(lines) => Right(lines)
    }
  }
}
