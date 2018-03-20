package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}


class AdvancedImplicitsSpec extends FunSpec with Matchers {

  describe(
    """Implicits is like a Map[Class[A], A] where A is any object and it is tied into the scope,
      | and it is there when you need it, hence it is implicit. This provide a lot of great techniques that we
      | can use in Scala.""".stripMargin) {

    it(
      """is done per scope so in the following example, we will begin with an implicit value
        |  and call it from inside a method which uses a multiple parameter list where one
        |  one group would """.stripMargin) {
      implicit val rate = 100

      def calculatePayment(hours: Int)(implicit currentRate: Int) = hours * rate

      calculatePayment(50) should be(5000)
    }

    it(
      """will allow you to place something manually, if you want to override the implicit value"""
        .stripMargin) {
      implicit val rate: Int = 100
      //implicit val age:Int = 20

      def calculatePayment(hours: Int)(implicit currentRate: Int) = hours * rate

      calculatePayment(50) should be(5000)
    }

    it(
      """will gripe at compile time if there are two implicit bindings of the same type.  It's
        |  worth noting that what Scala doing are compile time tricks for implicit.
        |  One strategy is to wrap a value in a type to avoid conflict"""
        .stripMargin) {
      case class Rate(value: Int)
      case class Age(value: Int)

      implicit val rate: Rate = Rate(100)
      implicit val age: Age = Age(40)

      def calculatePayment(hours: Int)(implicit currentRate: Rate) = {
        def methodA(x: Int) = x + (hours * currentRate.value)

        methodA(10)
      }
    }

    it(
      """is really used to bind services that require something and
        |  you don't particularly need to inject everywhere explicitly, in this
        |  case let's discuss Future[+T]""".stripMargin) {

      //import java.util.concurrent.Executors

      import scala.concurrent._
      import scala.concurrent.duration._
      import scala.language.postfixOps
      import scala.util.{Failure, Success}
      import scala.concurrent.ExecutionContext.Implicits.global

      //val executorService = Executors.newCachedThreadPool()
      //implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(executorService)

      val future: Future[Int] = Future {
        println(Thread.currentThread().getName)
        Thread.sleep(4000)
        100 * 4
      }

      //future.foreach(x => println(x))

      future.onComplete {
        case Success(v: Int) => println(s"Answer is $v")
        case Failure(e: Throwable) => println(
          s"Something bad happened ${e.getMessage}")
      }

      Await.ready(future, 5 seconds) //Block (Not a good idea in production)
    }

    it(
      """can bring up any implicit directly by merely calling up implicitly""")
    {
      case class IceCream(name: String)
      case class Scoops(n: Int, flavor: IceCream)
      implicit val flavorOfTheDay: IceCream = IceCream("Spring Green")

      def orderIceCream(num: Int)(implicit flavorOfTheDay: IceCream) = {
        Scoops(num, flavorOfTheDay)
      }

      orderIceCream(3)(IceCream("Rocky Road"))

      def orderIceCream2(num: Int) = {
        Scoops(num, implicitly[IceCream])
      }

      def orderIceCream3(n: Int, flavor: IceCream = implicitly[IceCream]) = {
        Scoops(n, flavor)
      }
    }

    it(
      """the implicit group parameter list, can contain more than one parameter, but
        |  needs to be in the same implicit parameter group""".stripMargin) {
      implicit val bonus = 5000
      implicit val currency = "Euro"

      def calcYearRate(amount: Int)
                      (implicit bonusAmt: Int, preferredCurrency: String) = {
        amount + bonusAmt + " " + preferredCurrency
      }

      calcYearRate(60000) should be("65000 Euro")
    }

    it( """can also be replaced with default parameters, choose accordingly""")
    {
      def calcYearRate(amount: Int, bonusAmt: Int = 5000,
                       currency: String = "Euro") = {
        amount + bonusAmt + " " + currency
      }

      calcYearRate(60000) should be("65000 Euro")
    }


    it(
      """Christopher A. Question: if you have a List[String] implicitly will it try
        | to inject into a List[Double]?""".stripMargin) {

      implicit val listOfString = List("Foo", "Bar", "Baz")
      implicit val listOfDouble = List(1.0, 2.0, 3.0)

      val result = implicitly[List[Double]]

      result should be(List(1.0, 2.0, 3.0))
    }


    it(
      """can be used for something like what Ruby has called
        |  monkey patching or Groovy calls mopping where we can add functionality to
        |  a class that we don't have access to, like isOdd/isEven
        |  in the Int class.  This is what we call implicit wrappers.
        |  First we will use a conversion method.""".stripMargin) {


      //Adapter
      class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      //Map[Class[A], A]
      //Key              Value
      //Int=>IntWrapper  [function]

      import scala.language.implicitConversions //I know what I am doing.

      //scastie.scala.org
      implicit def int2IntWrapper(x: Int): IntWrapper = new IntWrapper(x)

      40.isOdd should be(false)
      25.isOdd should be(true)
      10.isEven should be(true)
      13.isEven should be(false)
    }


    it(
      """Implicit wrappers can be created using a function and is often easier to mental map."""
        .stripMargin) {
      class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      //Map[Class[A], A]
      //Key              Value
      //Int=>IntWrapper  [function]

      import scala.language.implicitConversions
      //scastie.scala.org
      implicit val int2IntWrapper: Int => IntWrapper = (x: Int) => new IntWrapper(
        x)

      40.isOdd should be(false)
      25.isOdd should be(true)
      10.isEven should be(true)
      13.isEven should be(false)
    }

    it(
      """can be use a short hand version of this called implicit classes, before using them
        |  there are some rules:
        |  1. They can only be used inside of an object/trait/class
        |  2. They can only take one parameter in the constructor
        |  3. There can not be any colliding method name as that
        |     with the implicit outer scope""".stripMargin) {

      import scala.language.implicitConversions

      implicit class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = !isOdd
      }

      40.isOdd should be(false)
      25.isOdd should be(true)
      10.isEven should be(true)
      13.isEven should be(false)
    }

    it("is Hilesh's question with two isOdd and two isEven, ambigous method") {
      class IntWrapper(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = x % 2 == 0
      }

      implicit def int2IntWrapper(x: Int) = new IntWrapper(x)

      class IntChecker(x: Int) {
        def isOdd: Boolean = x % 2 != 0

        def isEven: Boolean = x % 2 == 0
      }

      implicit def int2IntChecker(x: Int) = new IntChecker(x)

      //10.isEven this will not work
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a method""".stripMargin) {

      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(value: Int) extends Currency
      case class Yen(value: Int) extends Currency

      implicit def int2Dollar(i: Int): Dollar = Dollar(i)

      def addAmount(x: Dollar, y: Dollar): Dollar = Dollar(x.value + y.value)

      addAmount(Dollar(40), Dollar(100)) should be(Dollar(140))

      addAmount(50, 150) should be(Dollar(200))
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a function""".stripMargin) {
      pending
    }

    it(
      """is done automatically in Scala because what is inside of scala.Predef, for example,
        |  it explains how be can set a scala.Float , and there is java.lang.Float,
        |  java primitive float.
        |  We can investigate this by looking at
        |  the documentation.""".stripMargin) {

      val f1: scala.Float = 4001.00f
      val f2: scala.Float = 5030.40f

      val result = java.lang.Math.min(f1, f2)

      result should be(4001.00f)
    }
  }

  describe("Locating implicits recipes") {
    it(
      """has a common way, to store that particular implicit
        |  recipe in an object that makes should make
        |  sense and then import that object""".stripMargin) {

      object MyPredef {

        import scala.language.implicitConversions

        implicit class IntWrapper(x: Int) {
          def isOdd: Boolean = x % 2 != 0

          def isEven: Boolean = !isOdd
        }

      }

      import MyPredef._
      29.isEven should be(false)
    }

    it(
      """can also use a companion object to store any implicit recipes"""
        .stripMargin) {
      class Artist(val firstName: String, val lastName: String)
      object Artist {

        import scala.language.implicitConversions

        implicit def tupleToArtist(t: (String, String)): Artist = new Artist(
          t._1, t._2)
      }

      def playPerformer(a: Artist) = s"Playing now: ${a.firstName} ${a.lastName}"

      playPerformer("Elvis" -> "Presley") should
        be("Playing now: Elvis Presley")
    }

    it( """can also use a package object to store some of these implicits""") {
      def numItems(list: List[String]) = list.mkString(",")

      numItems(3 -> "Whoa") should be("Whoa,Whoa,Whoa")
    }

    it(
      """can use JavaConverters to convert a collection in Java to Scala and vice versa""")
    {

      import scala.collection.JavaConverters._
      import java.time.ZoneId
      ZoneId.getAvailableZoneIds
        .asScala
        .toSet
        .filter(_.startsWith("Asia"))
        .map(_.split("/").last)
        .toList
        .size
    }
  }

  describe(
    "View Bounds are used to ensure that there is a particular recipe for a certain type")
  {
    it(
      """Uses <% inside of a parameterized type declaration to determine if there is a conversion available
        | then within you can treat an object as an object of that type. It is unorthodox, and has since been
        | deprecated.""".stripMargin) {

      import scala.language.implicitConversions

      class Employee(val firstName: String, val lastName: String)

      implicit def str2Employee(s: String): Employee = {
        val tokens = s.split(" ")
        new Employee(tokens(0), tokens(1))
      }

      def hireEmployee[A <% Employee](a: A) = {
        s"Hired an employee ${a.firstName} ${a.lastName}"
      }

      hireEmployee("Joe Employee") should be("Hired an employee Joe Employee")
    }
  }

  describe(
    """Context Bounds works so that there is a type A, and it requires a B[A] somewhere
      |  within the the implicit scope, for example like Ordered[T], or TypeTag[T], or Numeric[T],
      |  this provides a way to check that something is something can be implicitly defined but
      |  gives the end user no opportunity to the ability to inject a different implementation"""
      .stripMargin) {

    it(
      """uses the signature [T:WrappedType], which is
        | equivalent to (t:T)(implicit w:WrappedType[T])
        | let's try it with """.stripMargin) {

      trait Loggable[T] {
        def log(t: T): String
      }

      class Employee(val firstName: String, val lastName: String)

      implicit val loggableEmployee = new Loggable[Employee] {
        override def log(t: Employee): String = {
          s"Employee: ${t.firstName} ${t.lastName}"
        }
      }

      def writeItOut[T: Loggable](t: T) = {
        val loggable = implicitly[Loggable[T]]
        loggable.log(t)
      }

      writeItOut(new Employee("Roy", "Orbison")) should
        be("Employee: Roy Orbison")
    }
  }

  describe(
    """Type Constraints are used to ensure that a particular method can run
      | if a particular generic is of a certain type, this is typically used for
      | one method""".stripMargin) {

    it(
      """uses one operator, =:= which is actually the full type =:=[A,B] that
        |  will to see if something is of the same type""".stripMargin) {

      class MyPair[A, B](val a: A, val b: B) {
        def first: A = a

        def second: B = b

        def toList(implicit ev: A =:= B): List[A] = List(a, b)
          .asInstanceOf[List[A]]
      }

      val myPairHomogeneous = new MyPair(4, 10)
      myPairHomogeneous.toList should be(List(4, 10))

      val myPairHeterogeneous = new MyPair(4, "Foo")
      //I do not want the follow to work
      //myPairHeterogeneous.toList //compile error
    }

    it("""uses the operator, <:< which will test if A is a subtype of B""") {
      List(1 -> "One", 2 -> "Two").toMap
    }
  }

  describe("Getting around Erasure Using TypeTags") {
    it(
      "used to use Manifest but now uses a type tag to retrieve what is erased")
    {

      import scala.reflect.runtime.universe._

      def matchList[A](list: List[A])(implicit tt: TypeTag[A]): String = {
        tt.tpe match {
          case x if x =:= typeOf[String] => "List of String"
          case y if y =:= typeOf[Int] => "List of Int"
          case _ => "List of Something Else"
        }
      }

      matchList(List(1, 2, 3)) should be("List of Int")
    }
  }

  describe(
    """Typeclasses are a way of generating or extending behavior using Java-like interfaces,
      |  but operate as outside.  There is another term for this,
      |  and it's called ad-hoc polymorphism""".stripMargin) {

    it(
      """can be used to determine equality, so whether than make equals inside of an class,
        | it is now an outside concern""".stripMargin) {
      trait Eq[T] {
        def myEquals(a: T, b: T): Boolean
      }

      implicit val eqTypeClass: Eq[Team] = new Eq[Team] {
        override def myEquals(a: Team, b: Team): Boolean = {
          a.city == b.city &&
            a.manager == b.manager &&
            a.mascot == b.mascot
        }
      }

      def isEquals[A](a: A, b: A)(implicit eq: Eq[A]) = eq.myEquals(a, b)

      class Team(val mascot: String, val city: String, val manager: String)

      val a1 = new Team("Blue Jays", "Toronto", "Bobby Filet")
      val a2 = new Team("Blue Jays", "Toronto", "Bobby Filet")
      val a3 = new Team("Nationals", "Washington", "Carla Annapolis")

      isEquals(a1, a2) should be(true)
    }

    it("can be used for ordering") {
      case class Employee(firstName: String, lastName: String)

      object EmployeeOrdering {
        implicit val orderEmployeesByFirstName: Ordering[Employee] = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int = {
            x.firstName.compareTo(y.firstName)
          }
        }
        implicit val orderEmployeesByLastName: Ordering[Employee] = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int = {
            x.lastName.compareTo(y.lastName)
          }
        }
      }

      val list: List[Employee] = List(
        Employee("Adam", "Ant"),
        Employee("Samuel", "Jackson"),
        Employee("Janice", "Joplin"),
        Employee("Jimmy", "Page"),
        Employee("The", "Edge"),
        Employee("Scarlett", "Johansson"),
        Employee("Justin", "Bieber"))

      import EmployeeOrdering.orderEmployeesByLastName

      list.sorted.head should be(Employee("Adam", "Ant"))
    }
  }
}
