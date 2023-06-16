trait MySet[A] extends (A => Boolean) {

  /*
    EXERCISE - implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A] // union

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
   EXERCISE #2
   - removing an element
   - intersection with another set
   - difference with another set
   */
  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A] // difference
  def &(anotherSet: MySet[A]): MySet[A]  // intersection

  // EXERCISE #3 - implement a unary_! = NEGATION of a set
  // set[1,2,3] =>
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NoEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

class NoEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = this.head == elem || this.tail.contains(elem)

  override def +(elem: A): MySet[A] = if (elem == this.head) this else new NoEmptySet[A](head = elem, tail = this)

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] = if (head == elem) tail else tail - elem + head

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filterTail = tail.filter(predicate)
    if (predicate(head)) filterTail + head
    else filterTail
  }

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this(x))
}

class PropertyBasedSet[A](predicate: A => Boolean) extends MySet[A] {

  override def contains(elem: A): Boolean = predicate(elem)

  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => x == elem || predicate(x))

  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => anotherSet.contains(x) || predicate(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")

  override def filter(f: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => predicate(x) && f(x))

  override def foreach(f: A => Unit): Unit = politelyFail

  override def -(elem: A): MySet[A] = new PropertyBasedSet[A](x => predicate(x) && x != elem)

  override def --(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => predicate(x) && !anotherSet(x))

  override def &(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => predicate(x) && anotherSet(x))

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !predicate(x))
}
