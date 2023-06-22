object Monads extends App {
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  case class Success[+A](message: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try f(message)
      catch {
        case e: Throwable => Failed(e)
      }
  }

  case class Failed(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  //lazy monad
  class Lazy[+A](value: => A) {
    lazy val internalValue = value

    def map[B](f: A => B) = flatMap(x => Lazy(f(x)))

    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)

    def flatten(x: Lazy[Lazy[A]]): Lazy[A] = x.flatMap(x => x)
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try Success(a)
      catch {
        case e: Throwable => Failed(e)
      }
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

}
