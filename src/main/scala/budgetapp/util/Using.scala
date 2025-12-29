package budgetapp.util

object Using {
  // Tiny "Using" helper (Scala 2.12 friendly)
  def apply[A <: AutoCloseable, B](resource: A)(f: A => B): Either[Throwable, B] =
    try Right(f(resource))
    catch { case t: Throwable => Left(t) }
    finally {
      try resource.close()
      catch { case _: Throwable => () }
    }
}
