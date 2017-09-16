package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.MatchIterator

class AdvancedPatternMatchingSpec extends FunSpec with Matchers {

  //First review the basics

  describe("Simple Pattern Matches") {
    it("can be as simple as an assignment") {
      val x: Int = 42
      x should be(42)
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
      a should be(List())
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
      val fst :: snd :: thd :: Nil = List(1, 4, 10)
      fst should be(1)
      snd should be(4)
      thd should be(10)
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
      val List(fst, snd, _*) = (1 to 5).toList
      fst should be(1)
      snd should be(2)
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
      a[scala.MatchError] should be thrownBy {
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
          case n => item :: replicate(n - 1, item)
        }
      }

      replicate(0, "Foo") should be(List[String]())
      replicate(1, 5.0) should be(List(5.0))
      replicate(3, 5) should be(List(5, 5, 5))
      replicate(5, "Foo") should be(List("Foo", "Foo", "Foo", "Foo", "Foo"))
      replicate(2, "Foo") should be(List("Foo", "Foo"))
    }

    it( """will create a replicate in a tail recursive manner""".stripMargin) {
      def replicate[A](count: Int, item: A): List[A] = {
        @tailrec
        def repl(count: Int, acc: List[A]): List[A] = {
          count match {
            case 0 => acc
            case n => repl(n - 1, item :: acc)
          }
        }

        repl(count, Nil)
      }

      replicate(0, "Foo") should be(List[String]())
      replicate(1, 5.0) should be(List(5.0))
      replicate(3, 5) should be(List(5, 5, 5))
      replicate(5, "Foo") should be(List("Foo", "Foo", "Foo", "Foo", "Foo"))
      replicate(2, "Foo") should be(List("Foo", "Foo"))
    }


    it( """should show an empty list because we haven't covered that yet, this time with ::""") {
      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case _ :: y :: _ => Some(y)
          case _ => None
        }
      }

      mySecond(List()) should be(None)
      mySecond(List(1)) should be(None)
      mySecond(List(1, 2)) should be(Some(2))
      mySecond(List(1, 2, 3, 4, 5)) should be(Some(2))
      mySecond(List("Foo", "Bar", "Baz")) should be(Some("Bar"))
    }


    it( """should show an empty list because we haven't covered that yet.""") {
      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case List(_, y, _*) => Some(y)
          case _ => None
        }
      }

      mySecond(List()) should be(None)
      mySecond(List(1)) should be(None)
      mySecond(List(1, 2)) should be(Some(2))
      mySecond(List(1, 2, 3, 4, 5)) should be(Some(2))
      mySecond(List("Foo", "Bar", "Baz")) should be(Some("Bar"))
    }

    it( """can also use an alternative pipe to match""") {
      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case List() | List(_) => None
          case List(_, y, _*) => Some(y)
        }
      }

      mySecond(List()) should be(None)
      mySecond(List(1)) should be(None)
      mySecond(List(1, 2)) should be(Some(2))
      mySecond(List(1, 2, 3, 4, 5)) should be(Some(2))
      mySecond(List("Foo", "Bar", "Baz")) should be(Some("Bar"))
    }

    it(
      """should have a None in a pattern match, though we have not covered it.  This is just one way
        |  to get the information from an Option[T]""".stripMargin) {

      def number2String(x: Int): String = {
        val map = Map(1 -> "One", 2 -> "Two", 3 -> "Three", 4 -> "Four")
        map.get(x) match {
          case Some(a) => s"Number: $a"
          case None => "Nope"
        }
      }

      number2String(1) should be("Number: One")
      number2String(4) should be("Number: Four")
      number2String(3) should be("Number: Three")
      number2String(8) should be("Nope")
    }

    it( """should be careful with only Some vs. Option""") {
      val answerToEverything: Option[Int] = Some(42)
      val result = answerToEverything match {
        case Some(x) => s"The number is $x"
        case None => s"No Answer Available"
      }

      result should be("The number is 42")
    }

    it( """should also match just simple types like Int, String, etc.""") {
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

    it("""matching is used in exceptions""") {
      try {
        14 / 0
      } catch {
        case e: ArithmeticException => e.printStackTrace()
      }
    }

    it( """of course order is always important in pattern matching, particularly with types""") {
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

    it( """also works with a scala.collection.immutable.Stream""") {
      def from(x: Int): Stream[Int] = x #:: from(x + 1)

      def factorials(): Stream[Int] = from(1).scan(1)(_ * _)

      def secondStream[A](x: Stream[A]): Option[A] = {
        x match {
          case Stream.Empty => None // Shouldn't happen
          case _ #:: Stream.Empty => None //0!
          case _ #:: snd #:: rest => Some(snd) //1#
        }
      }

      secondStream(factorials()) should be(Some(1))
    }

    it( """should also have guards just in case""") {
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
      val Employee(fn, ln) :: Nil = List(Employee("Bertrand", "Russell"))
      fn should be("Bertrand")
      ln should be("Russell")
    }

    it( """can do compound matching where one item is in another, using the List() form""") {
      val List(Employee(fn, ln)) = List(Employee("Bertrand", "Russell"))
      fn should be("Bertrand")
      ln should be("Russell")
    }

    it(
      """can do compound matching layers deep, like an Employee,
        |  in a Some, in List, using the :: form""".stripMargin) {
      val Some(Employee(fn, ln)) :: Nil = List(Some(Employee("Mark", "Twain")))
      fn should be("Mark")
      ln should be("Twain")
    }

    it(
      """can do compound matching layers deep, like an Employee,
        |  in a Some, in List, using the List() form""".stripMargin) {
      val List(Some(Employee(fn, ln))) = List(Some(Employee("Mark", "Twain")))
      fn should be("Mark")
      ln should be("Twain")
    }

    it(
      """can be as simple as assignments with a custom case class and
        |  it must be a case class or class with an extractor""".stripMargin) {
      val Employee(fn, ln) = Employee("Harry", "Truman")
      fn should be("Harry")
      ln should be("Truman")
    }

    it(
      """can match the whole custom case class when provided with @
        |  along with the pattern match itself""".stripMargin) {
      val a@Employee(fn, ln) = Employee("Lisa", "Simpson")
      fn should be("Lisa")
      ln should be("Simpson")
      a should be(Employee("Lisa", "Simpson"))
    }

  }

  describe("A Regular Pattern Expression Match") {
    it("""uses .r after a String to Convert it to a Regex Type, from there groups can can be determined""".stripMargin) {
      case class PhoneNumber(ac: String, prefix: String, suffix: String)

      def convertStringToPhoneNumber(s: String): Option[PhoneNumber] = {
        val PhoneRegexPlain: Regex = """(\d{3})-(\d{4})""".r
        val AreaPhoneRegexPlain: Regex = """(\d{3})-(\d{3})-(\d{4})""".r

        s match {
          case PhoneRegexPlain(pre, suf) => Some(PhoneNumber("", pre, suf))
          case AreaPhoneRegexPlain(ac, pre, suf) => Some(PhoneNumber(ac, pre, suf))
          case _ => None
        }
      }

      convertStringToPhoneNumber("333-1230") should be(Some(PhoneNumber("", "333", "1230")))
      convertStringToPhoneNumber("404-333-1230") should be(Some(PhoneNumber("404", "333", "1230")))
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

      val result = List(1, 2, 3, 4, 5, 6).map {
        doubleEvens orElse tripleOdds
      }
      result should be(List(3, 4, 9, 8, 15, 12))
    }

    it( """can also be trimmed down inline with case statements compare the above with the following below""") {
      val result = List(1, 2, 3, 4, 5, 6).map { case x: Int if x % 2 == 0 => x * 2; case y: Int if y % 2 != 0 => y * 3 }
      result should be(List(3, 4, 9, 8, 15, 12))
    }
  }

  describe("Custom pattern matching") {

    it(
      """uses unapply to extract elements for a pattern match so you can do your own pattern matching,
        |  the unapply method should return an Option and either a tuple or list of the parts""".stripMargin) {
      pending
    }

    it( """while building a pattern match off of another unapply""".stripMargin) {
      pending
    }

    it( """can also be used in composing partial functions to form a complete function""") {
      pending
    }
  }

  describe("Custom pattern matching with an instance") {

    object Even {
      def unapply(arg: Int): Option[Int] = if (arg % 2 == 0) Some(arg) else None
    }

    object Odd {
      def unapply(arg: Int): Option[Int] = if (arg % 2 != 0) Some(arg) else None
    }

    it(
      """can also extract from an instance just in case it is the instance that contains logic
        |  to extract information, this is the technique used to for regex grouping""".stripMargin) {

      val num = 40
      val result = num match {
        case Even(_) => "Even"
        case Odd(_) => "Odd"
      }
      result should be("Even")
    }
  }

  describe("Custom pattern matching with unapplySeq") {
    it("would require an unapplySeq for extracting collections") {

      object WordNumbers {
        def unapplySeq(x: String): Option[Seq[String]] = {
          val regex = """\d+""".r
          val matches = regex.findAllIn(x)
          Some(matches.toSeq)
        }
      }

      val result = "The score yesterday was 110 to 99" match {
        case (WordNumbers()) => "No numbers in the word"
        case (WordNumbers(x)) => s"One number: $x"
        case (WordNumbers(x, y)) => s"Two numbers: $x, $y"
        case (WordNumbers(x, y, z@_*)) => s"Two or more numbers: $x, $y, and the rest is $z"
      }

      result should be("Two numbers: 110, 99")

      val otherResult = "There is no score" match {
        case (WordNumbers()) => "No numbers in the word"
        case (WordNumbers(x)) => s"One number: $x"
        case (WordNumbers(x, y)) => s"Two numbers: $x, $y"
        case (WordNumbers(x, y, z@_*)) => s"Two or more numbers: $x, $y, and the rest is $z"
      }

      otherResult should be("No numbers in the word")
    }
  }

  describe("Companion Object Extractors") {
    class Genre(val name: String)
    object Genre {
      def unapply(arg: Genre): Option[String] = Some(arg.name)
    }

    class Movie(val title:String, val year:Int, val genre: Genre)
    object Movie {
      def unapply(arg: Movie): Option[(String, Int, Genre)] = Some(arg.title, arg.year, arg.genre)
    }

    it(
      """Companion objects will generally have the unapply or unapplySeq for classes, this also means
        |  that case classes create unapply automatically, but you can create or override your own
        |  particular rules""".stripMargin) {

          val movie = new Movie("Fifth Element", 1998, new Genre("Science Fiction"))
          val result = movie match {
            case Movie(title, year, Genre(genreName)) => s"The movie presented is of Genre $genreName"
            case _ => s"Unknown Genre"
          }
          result should be ("The movie presented is of Genre Science Fiction")
    }
  }
}













