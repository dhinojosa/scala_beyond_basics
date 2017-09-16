package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec
import scala.util.matching.Regex

class AdvancedPatternMatchingSpec extends FunSpec with Matchers {

  describe("Simple Pattern Matches") {
    it("can be as simple as an assignment") {
      val x: Int = 40 // x will take on the value of 40
      x should be(40)
    }

    it("can also be more meaningful as something like a tuple") {
      val (x, y) = (100, "Foo")
      x should be(100)
      y should be("Foo")
      //Use a REPL to prove the type
    }

    it( """has the ability to have a value name and an @
          |  to not only capture the individual items but the whole item""".stripMargin) {
      val t@(x, y) = (100, "Foo")
      x should be(100)
      y should be("Foo")
      t should be((100, "Foo"))
      //Use a REPL to prove this it
      //Change it to an array
    }

    it("can be used with an Optional, and often is used as such, let's do a Some") {
      val Some(x) = Some(100)
      x should be(100)
      //None doesn't necessarily contain anything. So it is tough to do by an assignment
    }

    it( """can use an _ to signify that you are not interested in a particular element,
          |  let's try a tuple first.""".stripMargin) {
      val (x, _, z) = (4, 400.22, "Foo") // I don't care about y.
      x should be(4)
      z should be("Foo")
    }

    it( """can use an _ to signify that you are not interested in a particular element,
          |  and in an Option[T] although there is no way to extract a value but if you
          |  want to ensure a shape, it would make sense""".stripMargin) {
      val Some(_) = Some(9)
    }

    it( """can use an _ even in an assignment, although, only if you wish match a particular
          |  shape""".stripMargin) {
      val (_: Int) = 40
    }


    it( """We can also match Lists by assignment, start off simple,
          |  this is a match on an empty list""".stripMargin) {
      val a@List() = List()
      a should be('empty)
    }

    it( """can also match using a variant form since Nil represents an empty list""") {
      val a@Nil = List()
      a should be('empty)
    }

    it( """can also match on an empty list, let's try the list form""") {
      val a@List() = List()
      a should be('empty)
    }

    it( """can also match a single item using the :: form""") {
      val head :: Nil = List(2)
      head should be(2)
    }

    it( """can also match a single item using the List() form""") {
      val List(x) = List(2)
      x should be(2)
    }

    it(
      """can do a list where you care about an *exact* number of items,
        |  let's try two using :: form""".stripMargin) {
      val fst :: snd :: Nil = List(40, 19)
      fst should be(40)
      snd should be(19)
      //Try it with just fst::snd only and what did it do?
    }

    it( """can do a list where you care about an *exact* number of items,
          |  now with List() form""".stripMargin) {
      val List(fst, snd) = List(40, 19)
      fst should be(40)
      snd should be(19)
    }

    it( """can do a list where you care about an *exact* number of items,
          |  let's try three using :: form""".stripMargin) {
      val fst :: snd :: trd :: Nil = List(40, 19, 60)
      fst should be(40)
      snd should be(19)
      trd should be(60)
      //Try it with just fst::snd only and what did it do?
    }

    it( """can do a list where you care about an *exact* number of items,
          |  let's try three using the List() form""".stripMargin) {
      val List(fst, snd, trd) = List(40, 19, 60)
      fst should be(40)
      snd should be(19)
      trd should be(60)
    }

    it( """can do a list where you want to capture any number of items and
          |  keep the remainder in an extra list using the :: form""".stripMargin) {
      val fst :: snd :: rest = (1 to 10).toList
      fst should be(1)
      snd should be(2)
      rest should be((3 to 10).toList)
    }

    it( """can do a list where you want to capture any number of items and
          |  keep the remainder in an extra list using the List() form""".stripMargin) {
      val List(fst, snd, rest@_*) = (1 to 5).toList
      fst should be(1)
      snd should be(2)
      rest should be((3 to 5).toList)
    }

    it( """can do a list where you want to capture any number of items and
          |  ignore the remainder in an extra list using the List() form""".stripMargin) {
      val List(fst, snd, _*) = (1 to 5).toList
      fst should be(1)
      snd should be(2)
    }

    it( """can also capture a list with @ to capture the entire list while
          |  at the same time capturing elements of a list and the remainder
          |  here using the :: form""".stripMargin) {
      val all@fst :: snd :: rest = (1 to 10).toList
      fst should be(1)
      snd should be(2)
      rest should be((3 to 10).toList)
      all should be((1 to 10).toList)
    }

    it( """can also capture a list with @ to capture the entire list while
          |  at the same time capturing elements of a list and ignoring remainder
          |  here using the :: form""".stripMargin) {
      val all@fst :: snd :: _ = (1 to 10).toList
      fst should be(1)
      snd should be(2)
      all should be((1 to 10).toList)
    }

    it( """can also capture a list with @ to capture the entire list while
          |  at the same time capturing elements of a list and the remainder
          |  here using the List() form""".stripMargin) {
      val all@List(fst, snd, rest@_*) = (1 to 10).toList
      fst should be(1)
      snd should be(2)
      rest should be((3 to 10).toList)
      all should be((1 to 10).toList)
    }


    it( """will always throw a Match Error if something doesn't match, for example here
          |  is an attempted match with a tuple""".stripMargin) {
      val z: Any = (1, 4.0, "Foo") //Hiding the type, which is important
      //Introduce without the MatchError
      a[MatchError] should be thrownBy {
        val (x, y) = z
      }
    }


    it( """is rarely used in assignments, it is used inside a match,
          |  which takes the following form, let's start with a Tuple 3
          |  and attempt to match""".stripMargin) {
      val item: Any = (1, 1.0, "Wow")
      //Covering our tracks making the matching do it's job (Show what happens with out it)
      val result = item match {
        case (x, y) => s"Tuple 2 with elements $x and $y"
        case (x, y, z) => s"Tuple 3 with elements $x, $y, and $z"
      }
    }

    it( """will throw a MatchError if the pattern match is not well covered,
          |  this time let's try it with a List and put the pattern match
          |  inside of a method.""".stripMargin) {
      def replicate[A](count: Int, item: A): List[A] = {
        (count,
          item) match {
          case (0, _) => List[A]()
          case (1, x) => List(x) //cover apply
          case (c, x) => x :: replicate(c - 1, x) //later tonight make this tail recursive
        }
      }
      val result = replicate(5, "Whoa")
      result should be("Whoa" :: "Whoa" :: "Whoa" :: "Whoa" :: "Whoa" :: Nil)
    }

    it( """will create a replicate in a tail recursive manner""".stripMargin) {
      def replicate[A](count: Int, item: A): List[A] = {
        @tailrec
        def repl(count: Int, item: A, res: List[A]): List[A] = {
          (count, item) match {
            case (0, _) => res
            case (1, x) => x :: res //cover apply
            case (c, x) => repl(c - 1, x, x :: res)
          }
        }
        repl(count, item, Nil)
      }
      val result = replicate(5, "Whoa")
      result should be("Whoa" :: "Whoa" :: "Whoa" :: "Whoa" :: "Whoa" :: Nil)
    }

    it( """should show an empty list because we haven't covered that yet.""") {

      // Try this first
      //       def mySecond[A](list:List[A]):A = {
      //         list match {
      //           case _ :: b :: Nil => b
      //         }
      //       }
      //       mySecond('a' :: 'b' :: Nil) should be ('b') //Show in steps..

      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case Nil => None
          case _ :: Nil => None
          case _ :: s :: Nil => Some(s)
          case _ :: s :: rest => Some(s)
        }
      }
      mySecond(List(1, 2, 3, 4)) should be(Some(2))
      mySecond(List(1)) should be(None)
      mySecond(Nil) should be(None)
    }

    it( """can also use an alternative pipe to match""") {
      def myPipeSecond[A](lst: List[A]): Option[A] = {
        lst match {
          case List() | List(_) => None
          case List(_, y) => Some(y)
          case List(_, y, rest@_*) => Some(y)
        }
      }
      val items = List("Eggs", "Milk", "Naan", "Limes")
      myPipeSecond(items) should be(Some("Milk"))
    }

    it( """should have a None in a pattern match, though we have not covered it.  This is just one way
          |  to get the information from an Option[T]""".stripMargin) {
      def number2String(x: Int): String = {
        val map = Map(1 -> "One", 2 -> "Two", 3 -> "Three")
        map.get(x) match {
          case Some(c) => s"Found answer it is $c"
          //variable shadowing if I use x!
          case None => "Found Nothing"
        }
      }
      number2String(1) should be("Found answer it is One")
      number2String(4) should be("Found Nothing")
    }


    it( """should be careful with only Some vs. Option""") {
      val answerToEverything = Some(42)
      answerToEverything match {
        case Some(x) =>
          "The answer is of course $x"
        //case None => "We must keep going" //Will not compile
      }

      //Note:
    }

    it( """should also match just simple types like Int, String, etc.""") {
      def whatDoIHave_?(a: Any): String = {
        a match {
          case a: Int => "Int"
          case b: String => "String"
          case c: (_, _) => "Tuple 2"
          case d: (_, _, _) => "Tuple 3"
          case e:
            Number => "Another number"
        }
      }
      whatDoIHave_?("Hello") should be("String")
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
          case _ #:: snd #:: Stream.Empty => Some(snd) //1!
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

    it( """can be as simple as assignments with a custom case class and
          |  it must be a case class or class with an extractor""".stripMargin) {
      val Employee(fn, ln) = Employee("Harry", "Truman")
      fn should be("Harry")
      ln should be("Truman")
    }

    it( """can match the whole custom case class when provided with @
          |  along with the pattern match itself""".stripMargin) {
      val a@Employee(fn, ln) = Employee("Lisa", "Simpson")
      fn should be("Lisa")
      ln should be("Simpson")
      a should be(Employee("Lisa", "Simpson"))
    }

  }

  describe("A Regular Pattern Expression Match") {
    it( """uses .r after a String to Convert it to a Regex Type, from there groups can
          |  can be determined""".stripMargin) {

      case class PhoneNumber(countryCode: String, areaCode: String, prefix: String, suffix: String)

      def convertString2PhoneNumber(pn: String): Option[PhoneNumber] = {
        val PhoneNumberPlainRegex: Regex = """(\d{3})-(\d{4})""".r
        val PhoneNumberWithAreaRegex: Regex = """(\d{3})-(\d{3})-(\d{4})""".r
        val PhoneNumberWithEverythingRegex: Regex = """(\d{1,3})-(\d{3})-(\d{3})-(\d{4})""".r
        pn match {
          case PhoneNumberPlainRegex(pre, suf) => Some(PhoneNumber("1", "000", pre, suf))
          case PhoneNumberWithAreaRegex(ac, pre, suf) => Some(PhoneNumber("1", ac, pre, suf))
          case PhoneNumberWithEverythingRegex(cc, ac, pre, suf) => Some(PhoneNumber(cc, ac, pre, suf))
          case _ => None
        }
      }
      convertString2PhoneNumber("1-303-202-4001") should be(Some(PhoneNumber("1", "303", "202", "4001")))
    }
  }

  // Pig Latin Exercise

  describe("Partial Functions") {
    it( """is like a function, but with an added method called isDefined.  isDefined() returns
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
    object Even {
      private def isEven(x: Int) = x % 2 == 0

      def unapply(i: Int): Option[Int] = if (isEven(i)) Some(i) else None
    }

    object Odd {
      private def isOdd(x: Int) = x % 2 != 0

      def unapply(i: Int): Option[Int] = if (isOdd(i)) Some(i) else None
    }

    //Setting up your own pattern matching
    it( """uses unapply to extract elements for a pattern match so you can do your own pattern matching,
          |  the unapply method should return an Option and either a tuple or list of the parts""".stripMargin) {
      val num = 40
      num match {
        case Even(x) => "Even"
        case Odd(x) => "Odd"
      }
    }

    it( """while building a pattern match off of another unapply""".stripMargin) {
      val r = (40, 120)
      val result = r match {
        case (Even(_), Even(_)) => "Two Evens"
        case (Even(_), Odd(_)) => "Even, Odd"
        case (Odd(_), Even(_)) => "Odd, Even"
        case (Odd(_), Odd(_)) => "Two Odds"
      }
      result should be("Two Evens")
    }

    it( """can also be used in composing partial functions to form a complete function""") {
      val result = List(1, 2, 3, 4, 5, 6).map { case Even(x) => x * 2; case Odd(y) => y * 3 }
      result should be(List(3, 4, 9, 8, 15, 12))
    }
  }

  describe("Custom pattern matching with an instance") {
    class AllInt[B](val g:(Int, Int) => Int) {
      val regex = """\d+""".r
      def unapply(s:String):Option[Int] = {
        val it = regex.findAllIn(s)
        if (it.isEmpty) None else {
          Some(regex.findAllIn(s).map(_.toInt).toList.reduce(g))
        }
      }
    }

    it("""can also extract from an instance just in case it is the instance that contains logic
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
    it("would require an unapplySeq for extracting collections") {
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

    class Movie(val title: String, val year: Int, val genre: Genre)
    object Movie {
      def unapply(arg: Movie): Option[(String, Int, Genre)] = Some(arg.title, arg.year, arg.genre)
    }

    it( """Companion objects will generally have the unapply or unapplySeq for classes, this also means
          |  that case classes create unapply automatically, but you can create or override your own
          |  particular rules""".stripMargin) {
      val movie = new Movie("The Fifth Element", 1998, new Genre("Science Fiction"))
      val result = movie match {
        case Movie(_, _, Genre(genreName)) => s"The movie presented is of genre: $genreName"
      }
      result should be("The movie presented is of genre: Science Fiction")
    }
  }
  //Review things list Option, List, and look at their unapply, and unapplySeq
}