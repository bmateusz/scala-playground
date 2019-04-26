object Morphisms {

  sealed trait List[+T]

  case object Nil extends List[Nothing] {
    override def toString: String = "Nil"
  }

  case class Cons[T](a: T, as: List[T]) extends List[T] {
    override def toString: String = s"$a : $as"
  }

  object List {
    def apply[T](elems: T*): List[T] = elems.foldRight(Nil: List[T]) {
      case (curr, acc) => Cons(curr, acc)
    }
  }

  implicit class Catamorphism[A, B](list: List[A]) {
    def catamorphism(b: B)(f: (A, B) => B): B = Morphisms.catamorphism[A, B](b)(f)(list)
  }

  def catamorphism[A, B](b: B)(f: (A, B) => B): List[A] => B = {
    def h(list: List[A]): B = list match {
      case Nil => b
      case Cons(a, as) => f(a, h(as))
    }

    h
  }

  implicit class Anamorphism[A, B](b0: B) {
    def anamorphism(p: B => Boolean)(g: B => (A, B)): List[A] = Morphisms.anamorphism[A, B](p)(g)(b0)
  }

  def anamorphism[A, B](p: B => Boolean)(g: B => (A, B)): B => List[A] = {
    def h(b: B): List[A] = {
      if (p(b)) Nil else {
        val (a, b2) = g(b)
        Cons(a, h(b2))
      }
    }

    h
  }

  implicit class Hylomorphism[A, B, C](a0: A) {
    def hylomorphism(c: C)(f: (B, C) => C)(p: A => Boolean)(g: A => (B, A)): C =
      Morphisms.hylomorphism[A, B, C](c)(f)(p)(g)(a0)
  }

  def hylomorphism[A, B, C](c: C)(f: (B, C) => C)(p: A => Boolean)(g: A => (B, A)): A => C = {
    def h(a: A): C = {
      if (p(a)) c else {
        val (b, a2) = g(a)
        f(b, h(a2))
      }
    }

    h
  }

  def paramorphism(b: Int)(f: (Int, Int) => Int): Int => Int = {
    def h(a: Int): Int = a match {
      case 0 => b
      case n => f(n - 1, h(n - 1))
    }

    h
  }

  def paramorphism[T](b: List[T])(f: (T, (List[T], List[T])) => List[T]): List[T] => List[T] = {
    def h(a: List[T]): List[T] = a match {
      case Nil => b
      case Cons(a, as) => f(a, (as, h(as)))
    }

    h
  }

}
