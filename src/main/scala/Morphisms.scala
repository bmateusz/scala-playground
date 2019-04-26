object Morphisms {

  trait List[+T]

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

  def main(args: Array[String]): Unit = {

    val nums: List[Int] = List(4, 6, 8, 10, 20, 44)
    println(s"Nums = $nums")

    // Catamorphisms

    val sum = nums.catamorphism(0)(_ + _)
    println(s"Sum = $sum")

    val length = nums.catamorphism(0) { case (_, n) => n + 1 }
    println(s"Length = $length")

    val filtered = nums.catamorphism(Nil: List[Int]) { case (a, as) => if (a % 4 == 0) Cons(a, as) else as }
    println(s"Filtered to modulo 4 = $filtered")

    val mapIncrementC = nums.catamorphism(Nil: List[Int]) { case (a, bs) => Cons(a + 1, bs) }
    println(s"Map incremented with Catamorphism = $mapIncrementC")

    // Anamorphisms

    val labels = List("a", "b", "c", "d")
    println(s"Labels = $labels")

    val zip = (nums, labels).anamorphism {
      case (as, bs) => as == Nil || bs == Nil
    } {
      case (Cons(a, as), Cons(b, bs)) => ((a, b), (as, bs))
      case _ => throw new Exception("Impossible because of the predicate")
    }
    println(s"Zipped = $zip")

    // in the example predicate was _ => false, and an infinite list
    val iterate = anamorphism[Int, Int](p => p > 10)(n => (n, n + 1))
    println(iterate(0))

    val mapIncrementA = nums.anamorphism((_: List[Int]) == Nil) {
      case Cons(a, as) => (a + 1, as)
      case _ => throw new Exception("Impossible because of the predicate")
    }
    println(s"Map incremented with Anamorphism = $mapIncrementA")

    // Hylomorphisms

    val factorial = hylomorphism[Int, Int, Int](1)(_ * _)(_ == 0)(n => (n, n - 1))
    val factorialAlt = 5.hylomorphism(1)((_: Int) * _)(_ == 0)(n => (n, n - 1))
    val factorialZero = 0.hylomorphism(1)((_: Int) * _)(_ == 0)(n => (n, n - 1))
    println(s"Factorial 5 = ${factorial(5)} or $factorialAlt")
    println(s"Factorial 0 = $factorialZero")

    // Paramorphisms



  }

}
