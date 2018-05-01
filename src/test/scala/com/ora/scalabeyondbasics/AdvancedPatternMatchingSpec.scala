package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}

import scala.util.matching.Regex

class AdvancedPatternMatchingSpec extends FunSpec with Matchers {

  //First review the basics

  describe("Simple Pattern Matches") {
    it("can be as simple as an assignment") {
      val x: Int = 40
      x should be(40)
    }

    it("can also be more meaningful as something like a tuple") {
      val (x, y) = (100, "Foo")
      x should be(100)
      y should be("Foo")
    }

    it(
      """has the ability to have a value name and an @
        |  to not only capture the individual items
        |  but the whole item""".stripMargin) {
      val t@(x, y) = (100, "Foo")
      x should be(100)
      y should be("Foo")
      t should be(100 -> "Foo")
      t should be(100, "Foo")
    }

    it("can be used with an Option, " +
      "and often is used as such, let's do a Some") {
      val Some(x) = Some(100)
      x should be(100)
    }

    it(
      """can use an _ to signify that you
        |  are not interested in a particular element,
        |  let's try a tuple first.""".stripMargin) {
      val (x, _, z) = (4, 400.2, "Foo")
      x should be(4)
      z should be("Foo")
    }

    it(
      """can use an _ to signify that you are not interested in a particular element,
        |  and in an Option[T] although there is no way to extract a value but if you
        |  want to ensure a shape, it would make sense""".stripMargin) {
      val a@Some(_) = Some(100)
      a should be(Some(100))
    }

    it(
      """can use an _ even in an assignment,
        |  although, only if you wish match a particular
        |  shape""".stripMargin) {
      val a@(_: Int) = 40
      //val a:Int = 40
      a should be(40)
    }

    it(
      """We can also match Lists by assignment, start off simple,
        |  this is a match on an empty list""".stripMargin) {
      val a@List() = List()
      a should be('empty)
    }

    it(
      """can also match using a variant
        |form since Nil represents an empty list""".stripMargin) {
      val a@Nil = List()
      a should be('empty)
    }

    it( """can also match a single item using the :: form""") {
      val h :: Nil = List("Foo")
      h should be("Foo")
    }

    it( """can also match a single item using the List() form""") {
      val List(h) = List(10)
      h should be(10)
    }

    it(
      """can do a list where you care about an *exact*
        |  number of items,
        |  let's try two using :: form""".stripMargin) {
      val fst :: snd :: Nil = List(3, 10)
      fst should be(3)
      snd should be(10)
    }

    it(
      """can do a list where you care about an *exact* number of items,
        |  now with List() form""".stripMargin) {
      val List(fst, snd) = List(3, 10)
      fst should be(3)
      snd should be(10)
    }

    it(
      """can do a list where you care about an *exact* number of items,
        |  let's try three using :: form""".stripMargin) {
      val fst :: snd :: trd :: Nil = List(40, 19, 100)
      fst should be(40)
      snd should be(19)
      trd should be(100)
    }

    it(
      """can do a list where you care about an *exact* number of items,
        |  let's try three using the List() form""".stripMargin) {
      val List(fst, snd, trd) = 40 :: 19 :: 100 :: Nil
      fst should be(40)
      snd should be(19)
      trd should be(100)
    }

    it("""can do a list where you want to capture any number of items and
        |  keep the remainder in an extra list using the
        |  :: form""".stripMargin) {
      val f :: s :: xs = (1 to 5).toList
      f should be(1)
      s should be(2)
      xs should be(List(3, 4, 5))
    }

    it("""can do a list where you want to capture any number of items and
        |  keep the remainder in an extra list using the
        |  List() form""".stripMargin) {
      val List(f, s, rest@_*) = (1 to 5).toList
      f should be(1)
      s should be(2)
      rest should be(List(3, 4, 5))
    }

    it("""can do a list where you want to capture any number of items and
        |  ignore the remainder in an extra list using
        |  the List() form""".stripMargin) {
      val List(fst, snd, _*) = (1 to 5).toList
      fst should be(1)
      snd should be(2)
    }

    it("""can also capture a list with @ to capture the entire list while
        |  at the same time capturing elements of a list and the remainder
        |  here using the :: form""".stripMargin) {
      val fst :: snd :: _ = (1 to 5).toList
      fst should be(1)
      snd should be(2)
    }

    it(
      """can also capture a list with @ to capture the entire list while
        |  at the same time capturing elements of a list and ignoring remainder
        |  here using the :: form""".stripMargin) {
      val all@fst :: snd :: rest = (1 to 5).toList
      fst should be(1)
      snd should be(2)
      rest should be(List(3, 4, 5))
      all should be(List(1, 2, 3, 4, 5))
    }

    it(
      """can also capture a list with @ to capture the entire list while
        |  at the same time capturing elements of a list and the remainder
        |  here using the List() form""".stripMargin) {
      val all@List(fst, snd, rest@_*) = (1 to 10).toList
      fst should be(1)
      snd should be(2)
      rest should be(List(3, 4, 5, 6, 7, 8, 9, 10))
      all should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    }

    it(
      """will always throw a Match Error if
        |  something doesn't match, for example here
        |  is an attempted match with a tuple""".stripMargin) {
      val z: Any = (1, 4.0, "Foo")
      a[MatchError] should be thrownBy {
        val (x, y) = z
      }
    }

    it(
      """is rarely used in assignments, it is used inside a match,
        |  which takes the following form, let's start with a Tuple 3
        |  and attempt to match""".stripMargin) {
      val item: Any = (1, 1.0, "Wow")

      val result = item match {
        case (x, y) => s"Tuple 2 $x and $y"
        case (x, y, z) => s"Tuple 3 $x and $y and $z"
        case _ => s"That this is the default"
      }

      result should be("Tuple 3 1 and 1.0 and Wow")
    }

    it( """Lets do up a replicate method""".stripMargin) {
      def replicate[A](count:Int,a:A):List[A] =
        count match {
          case 0 => Nil
          case _ => a :: replicate(count - 1, a)
        }

      replicate(0, "Whoa") should be (List())
      replicate(1, "Whoa") should be (List("Whoa"))
      replicate(2, "Whoa") should be (List("Whoa", "Whoa"))
      replicate(5, "Whoa") should be (List("Whoa", "Whoa", "Whoa", "Whoa", "Whoa"))
    }

    it( """will create a replicate in a tail recursive manner""".stripMargin) {

      def replicate[A](count:Int, a:A):List[A] = {
        import scala.annotation.tailrec
        @tailrec
        def replHelper(c:Int, acc:List[A]):List[A] =
          c match {
            case 0 => acc
            case _ => replHelper(c - 1, a :: acc)
          }
        replHelper(count, Nil)
      }

      replicate(0, "Whoa") should be (List())
      replicate(1, "Whoa") should be (List("Whoa"))
      replicate(2, "Whoa") should be (List("Whoa", "Whoa"))
      replicate(5, "Whoa") should be (List("Whoa", "Whoa", "Whoa", "Whoa", "Whoa"))
    }


    it( """My Second implementation""") {
      def mySecond[A](xs:List[A]):Option[A] =
        xs match {
          case Nil => None
          case x :: Nil => None
          case x :: y :: rest => Some(y)
        }

      mySecond(Nil) should be (None)
      mySecond(List("Foo")) should be (None)
      mySecond(List(10, 12)) should be (Some(12))
    }

    it( """can also use an alternative pipe to match""") {
      def mySecond[A](xs:List[A]):Option[A] =
        xs match {
          case Nil | _ :: Nil => None
          case _ :: y :: _ => Some(y)
        }

      mySecond(Nil) should be (None)
      mySecond(List("Foo")) should be (None)
      mySecond(List(10, 12)) should be (Some(12))
    }

    it( """mySecond as concise as possible""") {
      def mySecond[A](xs:List[A]):Option[A] =
        xs match {
          case _ :: y :: _ => Some(y)
          case _ => None
        }

      mySecond(Nil) should be (None)
      mySecond(List("Foo")) should be (None)
      mySecond(List(10, 12)) should be (Some(12))
      mySecond(List('a', 'b', 'c', 'd')) should be (Some('b'))
    }

    it(
      """should have a None in a pattern match, though we have not
        |  covered it.  This is just one way
        |  to get the information from an Option[T]""".stripMargin) {
      def mySecond[A](xs:List[A]):Option[A] =
        xs match {
          case _ :: y :: _ => Some(y)
          case _ => None
        }

      val result = mySecond(List(1,2,3)) match {
        case Some(x) => s"The second element was $x"
        case None => "No second element"
      }

      result should be ("The second element was 2")
    }

    it( """should be careful with only Some vs. Option""") {
      val s: Option[Int] = Some(40)
      val result = s match {
        case Some(x) => s"Answer: $x"
        case None => "No Answer"
      }
      result should be ("Answer: 40")
    }

    it( """should also match just simple types like Int, String, etc.""") {
      val a:Any = 40

      val result = a match {
        case 0 => "ZERO!"
        case x:Int if x % 2 == 0 => s"Even Int" //guard
        case _:Int => s"Odd Int"
        case "Foo" => s"Foo!"
        case y:String => s"String: $y"
        case _ => "Unknown"
      }

      result should be ("Even Int")
    }

    it("""of course order is always important in pattern matching,
        |  particularly with types(see above)""".stripMargin) {
      pending
    }

    it( """also works with a scala.collection.immutable.Stream""") {
      def mySecondStream[A](xs:Stream[A]):Option[A] =
        xs match {
          //case x #:: Stream.empty => //one element
          case _ #:: y #:: _ => Some(y)
          case _ => None
        }

      mySecondStream(Stream.empty[Int]) should be (None)
      mySecondStream(Stream.from(1)) should be (Some(2))
    }

    it( """should also have guards just in case (see above)""") {
      pending
    }
  }

  describe("A Pattern Match with the following custom class") {

    case class Employee(firstName:String, lastName:String)

    it("""can do compound matching where one item is in another,
        |  using the :: form""".stripMargin) {
      val Employee(fn, ln) :: Nil = List(Employee("Homer", "Simpson"))

      fn should be ("Homer")
      ln should be ("Simpson")
    }

    it("""can do compound matching where one item is in another,
        |  using the List() form""".stripMargin) {
      val List(Employee(fn, ln), Employee(fn2, ln2)) =
           List(Employee("Homer", "Simpson"), Employee("Mark", "Twain") )

      fn should be ("Homer")
      fn2 should be ("Mark")
    }

    it("""can do compound matching layers deep, like an Employee,
        |  in a Some, in List, using the :: form""".stripMargin) {

      val List(Some(Employee(fn, ln)), Some(Employee(fn2, ln2))) =
        List(Some(Employee("Homer", "Simpson")),
             Some(Employee("Mark", "Twain")))

      fn should be ("Homer")
      fn2 should be ("Mark")
    }

    it("""can do compound matching layers deep, like an Employee, in a Some,
        |  in List, using the List() form""".stripMargin) {
      pending
    }

    it(
      """can be as simple as assignments with a custom case class and
        |  it must be a case class or class with an extractor""".stripMargin) {
      pending
    }

    it(
      """can match the whole custom case class when provided with @
        |  along with the pattern match itself""".stripMargin) {
      pending
    }

  }

  describe("A Regular Pattern Expression Match") {
    it(
      """uses .r after a String to Convert it to a Regex Type, from
        |  there groups can can be determined""".stripMargin) {

      val AmericanTelephoneNumberRegex:Regex =
            """1?\s*\((\d{3})\)\s*(\d{3})-(\d{4})""".r
      val UKTelephoneNumberRegex:Regex =
            """\+44\s*(d{2})\s*(d{4})\s*(d{4})""".r

      val result = "1(505) 240-3023" match {
        case AmericanTelephoneNumberRegex(ac, pre, suf) =>
          s"American(ac: $ac, pre: $pre, suf: $suf)"
        case UKTelephoneNumberRegex(ac, pre, suf) =>
          s"UK(ac: $ac, pre: $pre, suf: $suf)"
      }

      result should be ("American(ac: 505, pre: 240, suf: 3023)")
    }
  }

  describe("Partial Functions") {
    it(
      """is like a function, but with an added method called isDefined.
        |  isDefined() returns true or false, it also has an `apply` method to
        |  invoke the function iff isDefined returns true.
        |  Partial Functions together should form a complete
        |  function.""".stripMargin) {


      val doubleEvens = new PartialFunction[Int, Int]() {
        override def isDefinedAt(x: Int): Boolean = x % 2 == 0

        override def apply(v1: Int): Int = v1 * 2
      }

      val tripleOdds = new PartialFunction[Int, Int]() {
        override def isDefinedAt(x: Int): Boolean = x % 2 != 0

        override def apply(v1: Int): Int = v1 * 3
      }

      val function = doubleEvens orElse tripleOdds
      function(3) should be (9)
    }

    it("""can also be trimmed down inline with case statements compare the
        |  above with the following below""".stripMargin) {
      val result = List(1,2,3)
                  .map{case x:Int if x % 2 == 0 => x * 2;
                       case y:Int if y % 2 != 0 => y * 3}
      result should contain inOrder(3,4,9)
    }
  }

  describe("Custom pattern matching") {

    //You can do this in a object or a class. but most of the time, you will
    //do it in an object

    object Even {
      def unapply(arg: Int): Option[Int] =
        arg match {
          case a:Int if a % 2 == 0 => Some(a)
          case _ => None
        }
    }

    object Odd {
      def unapply(arg: Int): Option[Int] =
        arg match {
          case 5 => None
          case a:Int if a % 2 != 0 => Some(a)
          case _ => None
        }
    }


    it("""uses unapply to extract elements for a pattern match so you can
        |  do your own pattern matching, the unapply method should return
        |  an Option and either a tuple or list of the parts""".stripMargin) {
      val num:Any = 40
      val result = num match {
        case Even(aa) => s"$aa is even"
        case Odd(bb) => s"$bb is odd"
      }

      result should be ("40 is even")
    }

    it("""while building a pattern match off of another
        |  unapply""".stripMargin) {

      val r: (Int, Int) = (100, 77)
      val result: String = r match {
        case (Even(_), Even(_)) => "Both even"
        case (Even(_), Odd(_)) => "One even, One odd"
        case (Odd(_), Even(_)) => "One odd, One even"
        case (Odd(_), Odd(_)) => "Both odd"
      }

      result should be ("One even, One odd")
    }

    it("Even Odd Match with a list") {
      val List(Even(x), Even(y)) = List(100, 40)
      x should be (100)
      y should be (40)
    }

    it("""can also be used in composing partial functions to form a
        |  complete function""".stripMargin) {
      pending
    }
  }

  describe("Custom pattern matching with an instance") {
    it("""can also extract from an instance just in case it is the
        |  instance that contains logic
        |  to extract information, this is the technique used to for
        |  regex grouping""".stripMargin) {
      class AllInts(f:(Int, Int) => Int) {
        val r:Regex = """\d+""".r
        def unapply(arg: String): Option[Int] = {
          r.findAllIn(arg).toList.map(_.toInt).reduceOption(f)
        }
      }

      val allIntsSum = new AllInts(_ + _)

      val result = "The score is 10 to 20 with 2 minutes left on the clock" match {
        case allIntsSum(r) => s"Total: $r"
        case _ => "Unknown"
      }

      result should be ("Total: 32")
    }
  }



  describe("Companion Object Extractors") {

    class Genre private (val name: String)
    object Genre {
      def apply(name:String) = new Genre(name)
      def unapply(arg:Genre):Option[String] = Some(arg.name)
    }

    class Movie private (val title: String, val year: Int, val genre: Genre)
    object Movie {
      def apply(title:String, year:Int, genre: Genre) =
        new Movie(title, year, genre)
      def unapply(arg: Movie): Option[(String, Int, Genre)] =
        Some(arg.title, arg.year, arg.genre) //detupling
    }

    it(
      """Companion objects will generally have the unapply or unapplySeq
        |  for classes, this also means
        |  that case classes create unapply automatically, but you can
        |  create or override your own
        |  particular rules""".stripMargin) {

      val movie:Any = Movie("The Fifth Element", 1998, Genre("Science Fiction"))

      val result = movie match {
        case Movie(t, y, Genre(gn)) => s"The genre is $gn"
        case _ => "I don't know"
      }

      result should be ("The genre is Science Fiction")


    }

    describe("Custom pattern matching with unapplySeq") {
      it("would require an unapplySeq for extracting collections") {
        object WordNumbers {
          val regex: Regex = """\d+""".r

          def unapplySeq(s:String):Option[Seq[Int]] =
            Some(regex.findAllIn(s).toSeq.map(_.toInt))
        }

        val string = "The lucky numbers are 100, 44, 33, and 220"

        val result = string match {
          case WordNumbers() => "No numbers"
          case WordNumbers(n) => "One number $n"
          case WordNumbers(n1, n2) => "Two numbers: $n1 and $n2"
          case WordNumbers(n1, n2, n3) => "Three numbers: $n1, $n2, $n3"
          case WordNumbers(n1, n2, n3, rest@_*) =>
            s"Minimum three numbers: $n1, $n2, $n3, and the rest is ${rest.mkString(",")}"
        }

        result should be ("Minimum three numbers: 100, 44, 33, and the rest is 220")
      }
    }
  }

  //Finally: Review things list Option, List, and look at their unapply, and unapplySeq
}
