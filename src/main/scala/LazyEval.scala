abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  def #::[B >: A](elem: B): MyStream[B]

  def ++[B >: A](another: MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]

  def takeAsList(n: Int): List[A]
}

class EmptyStream extends MyStream[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException()

  override def tail: MyStream[Nothing] = throw new NoSuchElementException

  override def #::[B >: Nothing](elem: B): MyStream[B] = new Cons[B](elem, this)

  override def ++[B >: Nothing](another: MyStream[B]): MyStream[B] = another

  override def foreach(f: Nothing => Unit): Unit = ()

  override def map[B](f: Nothing => B): MyStream[B] = this

  override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = this

  override def takeAsList(n: Int): List[Nothing] = Nil
}

class Cons[A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  override lazy val tail: MyStream[A] = tl
  override val head: A = hd

  override def isEmpty: Boolean = false

  override def #::[B >: A](elem: B): MyStream[B] = new Cons(elem, this)

  override def ++[B >: A](another: MyStream[B]): MyStream[B] = new Cons(head, tail ++ another)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A => B): MyStream[B] = new Cons[B](f(head), tail.map(f))

  override def flatMap[B](f: A => MyStream[B]): MyStream[B] = ???

  override def filter(predicate: A => Boolean): MyStream[A] = ???

  override def take(n: Int): MyStream[A] = ???

  override def takeAsList(n: Int): List[A] = ???
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = ???
}

object exercise extends App {

}
