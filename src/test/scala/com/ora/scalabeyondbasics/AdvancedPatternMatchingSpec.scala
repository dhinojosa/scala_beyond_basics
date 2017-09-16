package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.MatchIterator

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
      a should be('empty) //'empty is not a Scala idiom, purely ScalaTest property
    }

    it( """can also match a single item using the :: form""") {
      val h :: Nil = List("Foo")
      h should be("Foo")
    }

    it( """can also match a single item using the List() form""") {
      val List(h) = List("Foo")
      h should be("Foo")
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
      val f :: s :: rest = (1 to 5).toList
      f should be(1)
      s should be(2)
      rest should be(List(3, 4, 5))
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
      a[MatchError] should be thrownBy {
        //ScalaTest's way catching exception
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
        case _ => s"That this is the default" //Like default in Java
      }

      result should be("Tuple 3 1 and 1.0 and Wow")
    }

    it( """Lets do up a replicate method""".stripMargin) {
      def replicate[A](count: Int, item: A): List[A] = {
        val myTuple = (count, item)
        myTuple match {
          case (0, _) => List[A]()
          case (1, x) => List(x)
          case (c, x) => x :: replicate(c - 1, x)
        }
      }

      val result = replicate(5, "Whoa")
      result should be(List("Whoa", "Whoa", "Whoa", "Whoa", "Whoa"))
    }

    it( """will create a replicate in a tail recursive manner""".stripMargin) {
      def replicate[A](count: Int, item: A): List[A] = {
        @tailrec
        def repl(count: Int, item: A, res: List[A]): List[A] = {
          val myTuple = (count, item)
          myTuple match {
            case (0, _) => res
            case (1, x) => x :: res
            case (c, x) => repl(c - 1, x, x :: res)
          }
        }

        repl(count, item, List())
      }

      val result = replicate(5, "Whoa")
      result should be(List("Whoa", "Whoa", "Whoa", "Whoa", "Whoa"))
    }


    it( """should show an empty list because we haven't covered that yet.""") {
      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case Nil => None
          case _ :: Nil => None
          case _ :: snd :: _ => Some(snd)
        }
      }

      mySecond(Nil) should be(None)
      mySecond(List(1)) should be(None)
      mySecond(List(1, 4)) should be(Some(4))
      mySecond(List(1, 4, 10)) should be(Some(4))
    }

    it( """can also use an alternative pipe to match""") {
      def mySecond[A](list: List[A]): Option[A] = {
        list match {
          case Nil | _ :: Nil => None
          case _ :: snd :: _ => Some(snd)
        }
      }

      def mySecondLF[A](list: List[A]): Option[A] = {
        list match {
          case List() | List(_) => None
          case List(_, snd, _*) => Some(snd)
        }
      }

      mySecond(Nil) should be(None)
      mySecond(List(1)) should be(None)
      mySecond(List(1, 4)) should be(Some(4))
      mySecond(List(1, 4, 10)) should be(Some(4))
    }

    it(
      """should have a None in a pattern match, though we have not covered it.  This is just one way
        |  to get the information from an Option[T]""".stripMargin) {
      val map = Map(1 -> "One", 2 -> "Two", 3 -> "Three")

      def number2String(x: Int): String = {
        map.get(x) match {
          case Some(c) => c
          case None => "Not Found"
        }
      }

      number2String(1) should be("One")
      number2String(5) should be("Not Found")
    }

    it( """should be careful with only Some vs. Option""") {
      val answerToEverything = Option(42)
      val result = answerToEverything match {
        case Some(x) => s"Answer is: $x"
        case None => s"Got Nothing"
      }
    }

    it( """should also match just simple types like Int, String, etc.""") {
      pending
    }

    it( """of course order is always important in pattern matching, particularly with types""") {
      pending
    }

    it( """also works with a scala.collection.immutable.Stream""") {
      pending
    }

    it( """should also have guards just in case""") {
      pending
    }
  }

  describe("A Pattern Match with the following custom class") {
    it( """can do compound matching where one item is in another, using the :: form""") {
      pending
    }

    it( """can do compound matching where one item is in another, using the List() form""") {
      pending
    }

    it(
      """can do compound matching layers deep, like an Employee,
        |  in a Some, in List, using the :: form""".stripMargin) {
      pending
    }

    it(
      """can do compound matching layers deep, like an Employee, in a Some, in List, using the List() form""") {
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
    it("""uses .r after a String to Convert it to a Regex Type, from there groups can can be determined""".stripMargin) {
      val phoneNumber = "400-2015"

      val myRegex = """(\d{3})-(\d{4})""".r

      val result = phoneNumber match {
        case myRegex(pre, suf) => s"Prefix is $pre and suffix is $suf"
        case myRegex(pre, suf, _*) => s"More than I can handle"
        case _               => "Sorry"
      }

      val myRegex(pre, suf) = phoneNumber

      result should be ("Prefix is 400 and suffix is 2015")
    }
  }

  describe("Partial Functions") {
    it(
      """is like a function, but with an added method called isDefined.  isDefined() returns
        | true or false, it also has an `apply` method to invoke the function iff isDefined returns true.
        | Partial Functions together should form a complete function.""".stripMargin) {
      pending
    }

    it( """can also be trimmed down inline with case statements compare the above with the following below""") {
      pending
    }
  }

  describe("Custom pattern matching") {
    object Even {
      //Extractor
      def unapply(arg: Int): Option[Int] = {
        if (arg % 2 == 0) Some(arg) else None
      }
    }

    object Odd {
      def unapply(arg: Int): Option[Int] = {
        if (arg % 2 != 0) Some(arg) else None
      }
    }


    it(
      """uses unapply to extract elements for a pattern match so you can do your own pattern matching,
        |  the unapply method should return an Option and either a tuple or list of the parts""".stripMargin) {

      val num = 402
      val result = num match {
        case Even(x) => "I got an even!"
        case Odd(x) => "I got an odd!"
      }

      result should be("I got an even!")
    }

    it( """while building a pattern match off of another unapply""".stripMargin) {
      val t = (10, 131)

      val result = t match {
        case (Even(_), Even(_)) => "Both evens"
        case (Even(_), Odd(_)) => "One even, one odd"
        case (Odd(_), Even(_)) => "One odd, one even"
        case (Odd(_), Odd(_)) => "Both odd"
      }

      result should be("One even, one odd")
    }

    it("""can have both apply and unapply, yes, this is what a case class actually""") {
      class Employee(val firstName: String, val lastName: String)
      object Employee {
        def apply(fn: String, ln: String) = new Employee(fn, ln)

        def unapply(e: Employee): Some[(String, String)] = Some((e.firstName, e.lastName))
      }
    }


    it( """can also be used in composing partial functions to form a complete function""") {
      val doubleEvens: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
        override def isDefinedAt(x: Int): Boolean = ???

        override def apply(v1: Int): Int = ???
      }


      val triplingOdds: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
        override def isDefinedAt(x: Int): Boolean = ???

        override def apply(v1: Int): Int = ???
      }


      doubleEvens.orElse(triplingOdds)

      val result = (1 to 4).toList.map {
        case Even(x) => x * 2
        case Odd(x) => x * 3
      }


      result should be(List(3, 4, 9, 8))
    }
  }

  describe("Custom pattern matching with an instance") {
    class AllInt[B](val g: (Int, Int) => Int) {
      val regex = """\d+""".r

      def unapply(arg: String): Option[Int] = {
        val nums = regex.findAllIn(arg)
        if (nums.isEmpty) None
        else {
          Some(nums.map(s => s.toInt).reduce(g))
        }
      }
    }

    it(
      """can also extract from an instance just in case it is the instance that contains logic
        |  to extract information, this is the technique used to for regex grouping""".stripMargin) {

      val question = "What is the total of 100, 120, 99, 15, and 6?"
      val allIntsSum = new AllInt((x:Int, y:Int) => x + y)
      val result = question match {
        case allIntsSum(total) => s"The sum is $total"
        case _ => s"Nothing available"
      }
      result should be ("The sum is 340")
    }
  }

  describe("Custom pattern matching with unapplySeq") {
    it("would require an unapplySeq for extracting collections") {
      object WordNumbers {
        def unapplySeq(s:String):Option[Seq[Int]] = {
          val regex = """\d+""".r
          val result = regex.findAllIn(s)
          if (result.isEmpty) None else Some(result.toSeq.map(_.toInt))
        }
      }

      val result = "The score yesterday was 100 to 98" match {
        case WordNumbers(n) => s"Found one number and it was $n"
        case WordNumbers(n, n2) => s"Found two numbers and it was $n and $n2"
        case WordNumbers(n, n2, rest@_*) => s"Found more than two numbers and it was $n and $n2 and the rest was $rest"
      }

      result should be ("Found two numbers and it was 100 and 98")
    }
  }

  describe("Companion Object Extractors") {
    it(
      """Companion objects will generally have the unapply or unapplySeq for classes, this also means
        |  that case classes create unapply automatically, but you can create or override your own
        |  particular rules""".stripMargin) {
      pending
    }
  }

  //Finally: Review things list Option, List, and look at their unapply, and unapplySeq
}
