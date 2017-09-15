package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec
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
        |  to not only capture the individual items but the whole item""".stripMargin) {
      val t@(x, y) = (100, "Foo")
      x should be(100)
      y should be("Foo")
      t should be(100 -> "Foo")
      t should be(100, "Foo")
    }

    it("can be used with an Optional, and often is used as such, let's do a Some") {
      val Some(x) = Some(100)
      x should be(100)
    }

    it(
      """can use an _ to signify that you are not interested in a particular element,
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
      """can use an _ even in an assignment, although, only if you wish match a particular
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

    it( """can also match using a variant form since Nil represents an empty list""") {
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
      """can do a list where you care about an *exact* number of items,
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

    it(
      """can do a list where you want to capture any number of items and
        |  keep the remainder in an extra list using the :: form""".stripMargin) {
      val f :: s :: xs = (1 to 5).toList
      f should be(1)
      s should be(2)
      xs should be(List(3, 4, 5))
    }

    it(
      """can do a list where you want to capture any number of items and
        |  keep the remainder in an extra list using the List() form""".stripMargin) {
      val List(f, s, xs@_*) = (1 to 5).toList
      f should be(1)
      s should be(2)
      xs should be(List(3, 4, 5))
    }

    it(
      """can do a list where you want to capture any number of items and
        |  ignore the remainder in an extra list using the List() form""".stripMargin) {
      val List(f, s, _*) = (1 to 5).toList
      f should be(1)
      s should be(2)
    }

    it(
      """can also capture a list with @ to capture the entire list while
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
      """will always throw a Match Error if something doesn't match, for example here
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

      def replicate[A](count: Int, item: A): List[A] = {
        count match {
          case 0 => Nil
          case c => item :: replicate(c - 1, item)
        }
      }

      replicate(0, "Wow") should be(List())
      replicate(1, "Wow") should be(List("Wow"))
      replicate(2, "Wow") should be(List("Wow", "Wow"))
      replicate(5, "Wow") should be(List("Wow", "Wow", "Wow", "Wow", "Wow"))
    }

    it( """will create a replicate in a tail recursive manner""".stripMargin) {
      def replicate[A](count: Int, item: A): List[A] = {
        @tailrec
        def repl(count: Int, item: A, acc: List[A]): List[A] = {
          count match {
            case 0 => acc
            case c => repl(c - 1, item, item :: acc)
          }
        }

        repl(count, item, Nil)
      }

      replicate(0, "Wow") should be(List())
      replicate(1, "Wow") should be(List("Wow"))
      replicate(2, "Wow") should be(List("Wow", "Wow"))
      replicate(5, "Wow") should be(List("Wow", "Wow", "Wow", "Wow", "Wow"))
    }


    it( """should show an empty list because we haven't covered that yet.""") {
      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case Nil => None
          case _ :: Nil => None
          case _ :: snd :: _ => Some(snd)
        }
      }

      mySecond(List()) should be(None)
      mySecond(List(1)) should be(None)
      mySecond(List(1, 2)) should be(Some(2))
      mySecond(List("A", "B", "C")) should be(Some("B"))
    }

    it( """can also use an alternative pipe to match""") {
      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case Nil | _ :: Nil => None
          case _ :: snd :: _ => Some(snd)
        }
      }

      mySecond(List()) should be(None)
      mySecond(List(1)) should be(None)
      mySecond(List(1, 2)) should be(Some(2))
      mySecond(List("A", "B", "C")) should be(Some("B"))
    }

    it(
      """should have a None in a pattern match, though we have not covered it.  This is just one way
        |  to get the information from an Option[T]""".stripMargin) {

      def number2String(x: Int): String = {
        val map = Map(1 -> "One", 2 -> "Two")
        map.get(x) match {
          case Some(ans) => s"Found: $ans"
          case None => "Not Found"
        }
      }

      number2String(1) should be("Found: One")
      number2String(5) should be("Not Found")
    }

    it( """should be careful with only Some vs. Option""") {
      val answerToEverything: Option[Int] = Some(42)
      val result = answerToEverything match {
        case Some(ans) => s"The answer is of course $ans"
        case None => "No answer found"
      }
      result should be("The answer is of course 42")
    }

    it( """catch up: should also match just simple types like Int, String, etc.""") {
      def whatDoIHave_?(a: Any): String = {
        a match {
          case a: Int => "Int"
          case b: String => "String"
          case c: (_, _) => "Tuple 2"
          case d: (_, _, _) => "Tuple 3"
          case e: Number => "Another number"
        }
      }
      whatDoIHave_?("Hello") should be("String")
    }

    it( """catch up: of course order is always important in pattern matching, particularly with types""") {
      def whatDoIHave_?(a: Any): String = {
        a match {
          case a: Int => "Int"
          case b: String => "String"
          case e: Number => "Another number"
          case c: (_, _) => "Tuple 2"
          case d: (_, _, _) => "Tuple 3"
        }
      }
      whatDoIHave_?(10) should be("Int")
    }

    it( """catch up: also works with a scala.collection.immutable.Stream""") {
      def from(x: Int): Stream[Int] = x #:: from(x + 1)
      def factorials(): Stream[Int] = from(1).scan(1)(_ * _)
      def secondStream[A](x: Stream[A]): Option[A] = {
        x match {
          case Stream.Empty => None // Shouldn't happen
          case _ #:: Stream.Empty => None //0!
          case _ #:: snd #:: Stream.Empty => Some(snd) //1!
          case _ #:: snd #:: rest => Some(snd) //1#
        }
      }
      secondStream(factorials()) should be(Some(1))
    }

    it( """catch up: should also have guards just in case""") {
      def lowBar(x: AnyVal): Int = {
        x match {
          case a: Int if a < 100 => 10
          case b: Int if b > 100 => 20
          case c: Double if c < 10 => 30
          case _ => 40
        }
      }
      lowBar(1000.0F) should be(40)
    }
  }

  describe("A Pattern Match with the following custom class") {
    case class Employee(firstName: String, lastName: String)

    it( """can do compound matching where one item is in another, using the :: form""") {
      val Employee(fn, ln) :: Nil = List(Employee("Bob", "Rogers"))
      fn should be("Bob")
      ln should be("Rogers")

    }

    it( """can do compound matching where one item is in another, using the List() form""") {
      val List(Employee(fn, ln)) = List(Employee("Bob", "Rogers"))
      fn should be("Bob")
      ln should be("Rogers")
    }

    it(
      """can do compound matching layers deep, like an Employee,
        |  in a Some, in List, using the :: form""".stripMargin) {
      val Some(Employee(fn, ln)) :: Nil = List(Some(Employee("Bob", "Rogers")))
      fn should be("Bob")
      ln should be("Rogers")
    }

    it(
      """can do compound matching layers deep, like an Employee, in a Some, in List, using the List() form""") {
      val List(Some(Employee(fn, ln))) = List(Some(Employee("Bob", "Rogers")))
      fn should be("Bob")
      ln should be("Rogers")
    }

    it(
      """can be as simple as assignments with a custom case class and
        |  it must be a case class or class with an extractor""".stripMargin) {

      val Employee(fn, ln) = Employee("Bob", "Rogers")
      fn should be("Bob")
      ln should be("Rogers")
    }

    it(
      """can match the whole custom case class when provided with @
        |  along with the pattern match itself""".stripMargin) {
      val foo@Employee(fn, ln) = Employee("Bob", "Rogers")
      fn should be("Bob")
      ln should be("Rogers")
      foo should be(Employee("Bob", "Rogers"))
    }

  }

  describe("A Regular Pattern Expression Match") {
    it("""uses .r after a String to Convert it to a Regex Type, from there groups can can be determined""".stripMargin) {
      case class PhoneNumber(countryCode: String, areaCode: String, prefix: String, suffix: String)

      def convertStringToPhoneNumber(s: String): Option[PhoneNumber] = {
        val PlainPhoneNumberRegex: Regex = """(\d{3})-(\d{4})""".r //555-4044
        val AreaCodePhoneNumberRegex =
          """(\d{3})-(\d{3})-(\d{4})""".r //915-444-3912
        val AllPhoneNumberRegex =
          """(\d{1,3})-(\d{3})-(\d{3})-(\d{4})""".r

        s match {
          case PlainPhoneNumberRegex(pre, suf) => Some(PhoneNumber("1", "000", pre, suf))
          case AreaCodePhoneNumberRegex(ac, pre, suf) => Some(PhoneNumber("1", ac, pre, suf))
          case AllPhoneNumberRegex(cc, ac, pre, suf) => Some(PhoneNumber(cc, ac, pre, suf))
          case _ => None
        }
      }

      convertStringToPhoneNumber("444-1034") should be(Some(PhoneNumber("1", "000", "444", "1034")))
    }
  }

  describe("Partial Functions") {
    it(
      """is like a function, but with an added method called isDefined.  isDefined() returns
        | true or false, it also has an `apply` method to invoke the function iff isDefined returns true.
        | Partial Functions together should form a complete function.""".stripMargin) {


      val doubleEvens = new PartialFunction[Int, Int]() {
        override def isDefinedAt(x: Int): Boolean = x % 2 == 0

        override def apply(v1: Int): Int = v1 * 2
      }

      val tripleOdds = new PartialFunction[Int, Int]() {
        override def isDefinedAt(x: Int): Boolean = x % 2 != 0

        override def apply(v1: Int): Int = v1 * 3
      }

      val result = List(1, 2, 3, 4, 5).map(doubleEvens orElse tripleOdds)

      result should be(List(3, 4, 9, 8, 15))
    }

    it( """can also be trimmed down inline with case statements compare the above with the following below""") {
      val result = List(1, 2, 3, 4, 5).map {
        case x: Int if x % 2 == 0 => x * 2
        case y: Int if y % 2 != 0 => y * 3
      }

      result should be(List(3, 4, 9, 8, 15))
    }
  }

  describe("Custom pattern matching") {
    object Even {
      def unapply(arg: Int): Option[Int] = if (arg % 2 == 0) Some(arg) else None
    }

    object Odd {
      def unapply(arg: Int): Option[Int] = if (arg % 2 != 0) Some(arg) else None
    }

    it(
      """uses unapply to extract elements for a pattern match so you can do your own pattern matching,
        |  the unapply method should return an Option and either a tuple or list of the parts""".stripMargin) {
      val num = 50
      val result = num match {
        case Even(n) => s"Got even! $n"
        case Odd(n) => s"Got odd! $n"
      }

      result should be("Got even! 50")
    }

    it( """while building a pattern match off of another unapply""".stripMargin) {
      val t2 = (50, 121)
      val result = t2 match {
        case (Even(_), Odd(_)) => "Even, Odd"
        case (Odd(_), Odd(_)) => "Odd, Odd"
        case (Odd(_), Even(_)) => "Odd, Even"
        case (Even(_), Even(_)) => "Even, Even"
      }

      result should be ("Even, Odd")
    }

    it( """can also be used in composing partial functions to form a complete function""") {
      val result = List(1, 2, 3, 4, 5).map {
        case Even(x) => x * 2
        case Odd(y) => y * 3
      }

      result should be(List(3, 4, 9, 8, 15))
    }
  }

  describe("Custom pattern matching with an instance") {

    class AllInt[B](val g:(Int, Int) => Int) {
      val regex: Regex = """\d+""".r
      def unapply(s:String):Option[Int] = {
        val it = regex.findAllIn(s)
        if (it.isEmpty) None else {
          Some(regex.findAllIn(s).map(_.toInt).toList.reduce(g))
        }
      }
    }

    it(
      """catch up: can also extract from an instance just in case it is the instance that contains logic
        |  to extract information, this is the technique used to for regex grouping""".stripMargin) {
      val question = "What is the total of 100, 300, 22, 97, 230, 950, and 411?"
      val sumInt = new AllInt(_ + _)

      val result = question match {
        case sumInt(r) => s"Captured: $r"
        case _ => "Unknown"
      }

      result should be ("Captured: 2110")
    }
  }

  describe("Custom pattern matching with unapplySeq") {
    it("catch up: would require an unapplySeq for extracting collections") {
      object WordNumbers {
        def unapplySeq(x: String): Option[Seq[String]] = {
          val regex = """\d+""".r
          val matches = regex.findAllIn(x)
          if (matches.isEmpty) None else Some(matches.toSeq)
        }
      }

      val result = s"The score yesterday was 110 to 99" match {
        case (WordNumbers()) => s"No numbers"
        case (WordNumbers(n1, n2)) => s"Two numbers: $n1 and $n2"
        case (WordNumbers(n1, n2, n3@_*)) => s"More than two numbers: $n1, $n2, and the rest is $n3"
        case x => s"Unknown with $x"
      }
      result should be("Two numbers: 110 and 99")
    }
  }

  describe("Companion Object Extractors") {

    class Genre(val name: String)
    object Genre {
      def unapply(arg: Genre): Option[String] = Some(arg.name)
    }

    //This is what a case class does
    class Movie(val title: String, val year: Int, val genre: Genre)
    object Movie {
      def unapply(arg: Movie): Option[(String, Int, Genre)] = Some(arg.title, arg.year, arg.genre)
    }

    it(
      """Companion objects will generally have the unapply or unapplySeq for classes, this also means
        |  that case classes create unapply automatically, but you can create or override your own
        |  particular rules""".stripMargin) {

       val movie = new Movie("The fifth element", 1998, new Genre("Science Fiction"))
       val result = movie match {
         case Movie(_, _, Genre(n)) => s"Genre: $n"
       }
       result should be ("Genre: Science Fiction")
    }
  }

  //Finally: Review things list Option, List, and look at their unapply, and unapplySeq
}
