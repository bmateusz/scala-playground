import Morphisms._
import utest._

object MorphismsTest extends TestSuite {

  val nums: List[Int] = List(4, 6, 8, 10, 20, 44)
  val labels = List("a", "b", "c", "d")

  val tests = Tests {
    'list - {
      'toString - {
        List(1, 2, 3).toString ==> "1 : 2 : 3 : Nil"
      }
    }
    'catamorphism - {
      'sum - {
        catamorphism[Int, Int](0)(_ + _)(nums) ==> 92
      }
      'length - {
        nums.catamorphism(0) { case (_, n) => n + 1 } ==> 6
      }
      'filtered - {
        nums.catamorphism(Nil: List[Int]) { case (a, as) => if (a % 4 == 0) Cons(a, as) else as } ==> List(4, 8, 20, 44)
      }
      'increment - {
        nums.catamorphism(Nil: List[Int]) { case (a, bs) => Cons(a + 1, bs) } ==> List(5, 7, 9, 11, 21, 45)
      }
    }
    'anamorphism - {
      'zip - {
        val zip = (nums, labels).anamorphism {
          case (as, bs) => as == Nil || bs == Nil
        } {
          case (Cons(a, as), Cons(b, bs)) => ((a, b), (as, bs))
          case (_, _) => throw new Exception("Impossible because of the predicate")
        }

        zip ==> List((4, "a"), (6, "b"), (8, "c"), (10, "d"))
      }
      'iterate - {
        // in the example predicate was _ => false, and an infinite list
        val iterate = anamorphism[Int, Int](p => p > 10)(n => (n, n + 1))
        iterate(0) ==> List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      }
      'increment - {
        nums.anamorphism((_: List[Int]) == Nil) {
          case Cons(a, as) => (a + 1, as)
          case Nil => throw new Exception("Impossible because of the predicate")
        } ==> List(5, 7, 9, 11, 21, 45)
      }
    }
    'hylomorphism - {
      'factorial - {
        hylomorphism[Int, Int, Int](1)(_ * _)(_ == 0)(n => (n, n - 1))(5) ==> 120
      }
      'factorialAlt - {
        5.hylomorphism(1)((_: Int) * _)(_ == 0)(n => (n, n - 1)) ==> 120
      }
      'factorialZero - {
        0.hylomorphism(1)((_: Int) * _)(_ == 0)(n => (n, n - 1)) ==> 1
      }
    }
    'paramorphism - {
      'factorial - {
        paramorphism(1){case (n, m) => (n + 1) * m}(5) ==> 120
      }
      'tails - {
        val tails = paramorphism(Cons(Nil : List[Int], Nil : List[Int])){ case (a, (as, tls)) => Cons(Cons(a, as), tls) }
        tails(List(4, 6, 8)) ==> List(List(4, 6, 8), List(6, 8), List(8), Nil)
      }
    }
  }
}