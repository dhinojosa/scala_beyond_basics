package com.ora.scalabeyondbasics

import java.util.concurrent.Executors

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

      implicit val rate = 100 // Implicitly bound as [Int] with 10 as the value

      def calculatePayment(hours: Int)(implicit rate: Int) = hours * rate

      calculatePayment(50) should be(5000) //Notice there is no rate, because it will implicitly come in from scope
    }

    it("""will allow you to place something manually, if you want to override the implicit value""".stripMargin) {

      implicit val rate = 100
      //implicit val age = 40  If uncommented, will cause disruption

      def calculatePayment(hours: Int)(implicit rate: Int) = hours * rate

      calculatePayment(50)(110) should be(5500)
    }

    it(
      """will gripe at compile time if there are two implicit bindings of the same type.  It's
        |  worth noting that what Scala doing are compile time tricks for implicit. One strategy is to
        |  wrap a value in a type to avoid conflict""".stripMargin) {

      case class Rate(value: Int)
      case class Age(value: Int)

      implicit val rate = Rate(100)
      //implicit val age = 40  If uncommented, will cause disruption
      implicit val age = Age(40)

      def calculatePayment(hours: Int)(implicit rate: Rate) = hours * rate.value

      calculatePayment(50)
    }


    it(
      """is really used to bind services that require something and
        |  you don't particularly need to inject everywhere explicitly, in this
        |  case let's discuss Future[+T]""".stripMargin) {

      import scala.concurrent._

      val executionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

      val f = Future.apply {
        Thread.sleep(4000)
        100 + 40
      }(executionContext)

      f.foreach(a => println(a))(executionContext) //Clean this up!
    }


    it( """can bring up any implicit directly by merely calling up implicitly""") {
      case class IceCream(name: String)
      case class Scoops(num: Int, flavor: IceCream)
      implicit val flavorOfTheDay = IceCream("Holiday Cheer")

      def orderIceCream(num: Int)(implicit flavor: IceCream) = Scoops(num, flavor) //One way

      def orderIceCream2(num: Int): Scoops = {
        Scoops(num, implicitly[IceCream]) //hidden from a parameter entry
      }

      orderIceCream2(4) should be(Scoops(4, IceCream("Holiday Cheer")))
    }

    it(
      """the implicit group parameter list, can contain more than one paramter, but
        |  needs to be in the same implicit parameter group""".stripMargin) {
      implicit val bonus = 5000
      implicit val currency = "Euro"

      def calculateYearRate(amount: Int)(implicit bonus: Int, currency: String): String = {
        amount + bonus + " " + currency
      }

      calculateYearRate(60000) should be("65000 Euro")
    }

    it( """can also be replaced with default parameters, choose accordingly""") {
      def calculateYearRate(amount: Int, bonus: Int = 5000, currency: String = "Euro"): String = {
        amount + bonus + " " + currency
      }

      calculateYearRate(60000) should be("65000 Euro")
    }

    it(
      """Christopher A. Question: if you have a List[String] implicitly will it try
        | to inject into a List[Double]?""".stripMargin) {

      implicit val listOfString: List[String] = List("Foo", "Bar", "Baz")
      implicit val listOfDouble: List[Double] = List(1.0, 44.0, 3.1525)

      val result = implicitly[List[Double]]

      result should be(List(1.0, 44.0, 3.1525))
    }


    it(
      """can be used for something like what Ruby has called
        |  monkey patching or Groovy calls mopping where we can add functionality to
        |  a class that we don't have access to, like isOdd/isEven
        |  in the Int class.  This is what we call implicit wrappers.
        |  First we will use a conversion method.""".stripMargin) {

      class IntWrapper(x: Int) {
        def isOdd: Boolean = !isEven

        def isEven: Boolean = x % 2 == 0
      }

      import scala.language.implicitConversions
      //A flag to turn a feature on.

      implicit def int2IntWrapper(x: Int): IntWrapper = new IntWrapper(x)

      40.isOdd should be(false)
      40.isEven should be(true)
    }


    it( """Implicit wrappers can be created using a function and is often easier to mental map.""".stripMargin) {
      class IntWrapper(x: Int) {
        def isOdd: Boolean = !isEven

        def isEven: Boolean = x % 2 == 0
      }

      import scala.language.implicitConversions
      //A flag to turn a feature on.

      implicit val int2IntWrapper: Int => IntWrapper = x => new IntWrapper(x)

      40.isOdd should be(false)
      40.isEven should be(true)
    }


    it(
      """can be use a short hand version of this called implicit classes, before using them
        |  there are some rules:
        |  1. They can only be used inside of an object/trait/class
        |  2. They can only take one parameter in the constructor
        |  3. There can not be any colliding method name as that with the implicit outer scope""".stripMargin) {

      import scala.language.implicitConversions
      //A flag to turn a feature on.

      //We are already in a class, so this will work
      implicit class IntWrapper(x: Int) {
        def isOdd: Boolean = !isEven

        def isEven: Boolean = x % 2 == 0
      }

      40.isOdd should be(false)
      40.isEven should be(true)
    }

    it(
      """can also convert things to make it fit into a particular API, this is called implicit conversion,
        | in this scenario we will use a method""".stripMargin) {

      import scala.language.implicitConversions
      //A flag to turn a feature on.

      sealed abstract class Currency
      case class Dollar(value: Int) extends Currency
      case class Yen(value: Int) extends Currency

      implicit def int2Dollar(x: Int): Dollar = Dollar(x)

      def addAmounts(x: Dollar, y: Dollar) = Dollar(x.value + y.value)

      addAmounts(40, 1000) should be(Dollar(1040))
    }

    it(
      """can also convert things to make it fit into a particular API, this is called implicit conversion,
        | in this scenario we will use a function""".stripMargin) {
      import scala.language.implicitConversions
      //A flag to turn a feature on.

      sealed abstract class Currency
      case class Dollar(value: Int) extends Currency
      case class Yen(value: Int) extends Currency

      implicit val int2Dollar: Int => Dollar = x => Dollar(x)

      def addAmounts(x: Dollar, y: Dollar) = Dollar(x.value + y.value)

      addAmounts(40, 1000) should be(Dollar(1040))
    }

    it(
      """is done automatically in Scala because what is inside of scala.Predef, for example,
        |  it explains how be can set a Seq[Char] to a CharSequence. We can investigate this by looking at
        |  the documentation.""".stripMargin) {


      val f: scala.Float = 4001.00f
      val f2: scala.Float = 3011.00f

      val result = java.lang.Math.min(f, f2)

      result should be(3011.00f)
    }
  }

  describe("Locating implicits recipes") {
    it(
      """has a common way, to store that particular implicit recipe in an object that makes should make
        |  sense and then import that object""".stripMargin) {

      object MyPredef {

        implicit class IntWrapper(x: Int) {
          def isOdd: Boolean = !isEven

          def isEven: Boolean = x % 2 == 0
        }

      }

      import MyPredef._
      10.isOdd should be(false)
      10.isEven should be(true)
    }

    it( """can also use a companion object to store any implicit recipes""".stripMargin) {
      class Artist(val firstName: String, val lastName: String)
      object Artist {

        import scala.language.implicitConversions

        implicit def tupleToEmployee(t: (String, String)): Artist = new Artist(t._1, t._2)
      }

      def playPerformer(e: Artist) = s"Employee ${e.firstName} ${e.lastName} performing!"

      import Artist._

      playPerformer("Stevie" -> "Wonder") should be("Employee Stevie Wonder performing!")
    }

    it( """can also use a package object to store some of these implicits""") {
      def numItems(list: List[String]) = list.reduce(_ + _)

      numItems(3 -> "Whoa") should be("WhoaWhoaWhoa")
    }

    it("""can use JavaConverters to convert a collection in Java to Scala and vice versa""") {

      // Use REPL

      import scala.collection.JavaConverters._
      import java.time.ZoneId

      ZoneId.getAvailableZoneIds.asScala.toSet
        .filter(x => x.startsWith("Asia")).map(x => x.split("/").last).size
    }
  }

  describe("View Bounds are used to ensure that there is a particular recipe for a certain type") {
    it(
      """Uses <% inside of a parameterized type declaration to determine if there is a conversion available
        | then within you can treat an object as an object of that type. It is unorthodox, and has since been
        | deprecated.""".stripMargin) {

      class Employee(val firstName: String, val lastName: String)

      import scala.language.implicitConversions

      implicit def str2Employee(s: String): Employee = {
        s.split(" ").toList match {
          case Nil => new Employee("John", "Doe")
          case fn :: Nil => new Employee(fn, "Doe")
          case fn :: ln :: Nil => new Employee(fn, ln) //Not needed
          case fn :: ln :: _ => new Employee(fn, ln)
        }
      }

      //A -> Employee
      def hireEmployee[A <% Employee](s: A) = {
        s"HiredEmployee: ${s.firstName} ${s.lastName}"
      }

      hireEmployee("Joe Armstrong") should be("HiredEmployee: Joe Armstrong")
    }
  }

  describe(
    """Context Bounds works so that there is a type A, and it requires a B[A] somewhere
      |  within the the implicit scope, for example like Ordered[T], or TypeTag[T], or Numeric[T],
      |  this provides a way to check that something is something can be implicitly defined but
      |  gives the end user no opportunity to the ability to inject a different implementation""".stripMargin) {

    it(
      """uses the signature [T:WrappedType], which is equivalent to (t:T)(implicit w:WrappedType[T])
        |  let's try it with """.stripMargin) {

      trait Loggable[T] {
        def log(t: T)
      }

      class Employee(val firstName: String, val lastName: String)

      implicit val loggable: Loggable[Employee] = new Loggable[Employee] {
        override def log(t: Employee) = s"${t.firstName} ${t.lastName}"
      }

      def writeItOut[T: Loggable](t: T) = {
        val loggable = implicitly[Loggable[T]]
        loggable.log(t)
      }
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

        def toList(implicit x: A =:= B): List[A] = List(a, b).asInstanceOf[List[A]]
      }

      val myPair = new MyPair(4, 10)
      myPair.toList should be(List(4, 10))
    }

    it("""uses the operator, <:< which will test if A is a subtype of B""") {
      //See List.toMap
    }
  }

  describe("Getting around Erasure Using TypeTags") {

    it("used to use Manifest but now uses a type tag to retrieve what is erased") {
      //      def matchList[A](list: List[A]): String = {
      //        list match {
      //          case _: List[String] => "List of Strings"
      //          case _: List[Int] => "List of Ints"
      //          case _ => "List of Unknowns"
      //        }
      //      }
      //
      //      matchList(List(1, 2, 3, 4, 5)) should be("List of Ints") //Wrong

      import scala.reflect.runtime.universe._
      def matchList[A](list: List[A])(implicit tt: TypeTag[A]) = {
        tt.tpe match {
          case t if t =:= typeOf[Int] => "List of Ints"
          case t if t =:= typeOf[String] => "List of Strings"
          case _ => "List of Unknowns"
        }
      }
      matchList(List(1, 2, 3, 4, 5)) should be("List of Ints")
    }
  }

  describe( """Typeclasses are a way of generating or extending behavior using Java-like interfaces,
      |  but operate as outside.  There is another term for this,
      |  and it's called ad-hoc polymorphism""".stripMargin) {

    it( """can be used to determine equality, so whether than make equals inside of an class,
        | it is now an outside concern""".stripMargin) {

      class Employee(val firstName:String, val lastName:String)

      trait Eq[T] {
        def equals(a:T, b:T):Boolean
      }

      implicit val eqEmployee:Eq[Employee] = new Eq[Employee] {
        override def equals(a: Employee, b: Employee): Boolean = a.firstName == b.firstName &&
                                                         a.lastName == b.lastName
      }

      def equals[A](a:A, b:A)(implicit eq:Eq[A]) = eq.equals(a, b)

      val x = new Employee("John", "Ambrose")
      val y = new Employee("John", "Ambrose")

      equals(x, y) should be (true)
    }

    it("can be used for ordering") {
      case class Employee(firstName:String, lastName:String)

      implicit val employeeOrdering:Ordering[Employee] = new Ordering[Employee]{
        override def compare(x: Employee, y: Employee): Int = y.lastName.compareToIgnoreCase(x.lastName)
      }

      val sortedList = List(Employee("Carl", "Yemen"), Employee("Janice", "Algeria"),
                            Employee("Samuel", "Labrador"), Employee("Zoe", "Zanzibar")).sorted

      sortedList.head.lastName should be ("Zanzibar")
    }
  }
}
