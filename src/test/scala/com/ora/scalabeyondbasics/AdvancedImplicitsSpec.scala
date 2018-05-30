package com.ora.scalabeyondbasics

import org.scalatest.{FunSpec, Matchers}

class AdvancedImplicitsSpec extends FunSpec with Matchers {

  class Artist(val firstName:String, val lastName:String)
  object Artist {
    import scala.language.implicitConversions
    implicit def tuple2Artist(t:(String, String)): Artist = new Artist(t._1, t._2)
  }

  describe(
    """Implicits is like a Map[Class[A], A] where A is any object and it is tied into the scope,
      | and it is there when you need it, hence it is implicit. This provide a lot of great techniques that we
      | can use in Scala.""".stripMargin) {



    it(
      """is done per scope so in the following example, we will begin with an implicit value
        |  and call it from inside a method which uses a multiple parameter list where one
        |  one group would """.stripMargin) {

      implicit val rate: Int = 100

      def calcPayment(hours:Int)(implicit n:Int) = hours * n

      calcPayment(50) should be (5000)
    }

    it(
      """will allow you to place something manually,
        |if you want to override the implicit value""".stripMargin) {
      implicit val rate: Int = 100

      def calcPayment(hours:Int)(implicit rate:Int) = hours * rate

      calcPayment(50)(110) should be (5500)
    }

    it(
      """will gripe at compile time if there are two implicit bindings of the same type.  It's
        |  worth noting that what Scala doing are compile time tricks for implicit. One strategy is to
        |  wrap a value in a type to avoid conflict""".stripMargin) {

      case class Rate(value:Int)
      case class Age(value:Int)

      implicit val rate: Rate = Rate(100)
      implicit val age: Age = Age(40)

      def calcPayment(hours:Int)(implicit appleIsFruit:Rate) = hours * appleIsFruit.value

      calcPayment(50) should be (5000)
    }

    it(
      """is really used to bind services that require something and
        |  you don't particularly need to inject everywhere explicitly, in this
        |  case let's discuss Future[+T]""".stripMargin) {

      import scala.concurrent._
      import java.util.concurrent.Executors

      val executor = Executors.newFixedThreadPool(10)
      implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(executor)

      val future = Future {
        println(s"Thread-name: ${Thread.currentThread().getName}")
        Thread.sleep(3000)
        50 + 100
      }

      future.foreach(a => println(a))
    }


    it( """can bring up any implicit directly by merely calling up implicitly""") {
      case class IceCream(name: String)
      case class Scoops(num:Int, flavor:IceCream)

      implicit val flavorOfTheMonth: IceCream = IceCream("Rainbow Sherbet")

      def orderIceCream(num:Int) = {
        Scoops(num, implicitly[IceCream])
      }

      orderIceCream(4) should be (Scoops(4, IceCream("Rainbow Sherbet")))

    }

    it(
      """the implicit group parameter list, can contain more than one parameter, but
        |  needs to be in the same implicit parameter group""".stripMargin) {

      implicit val bonus = 1000
      implicit val currency = "Euro"

      def calcYearRate(amount:Int)(implicit bonus:Int, currency:String):String = {
        amount + bonus + " " + currency
      }

      calcYearRate(60000) should be ("61000 Euro")
    }

    it( """can also be replaced with default parameters, choose accordingly""") {

      def calcYearRate(amount:Int, bonus:Int = 1000, currency:String = "Euro"):String = {
        amount + bonus + " " + currency
      }

      calcYearRate(60000) should be ("61000 Euro")
    }


    it(
      """Christopher A. Question: if you have a List[String] implicitly will it try
        | to inject into a List[Double]?""".stripMargin) {

      implicit val listOfString = List("Foo", "Bar", "Baz")
      implicit val listOfDouble = List(1.0, 2.0, 3.0)


      val result = implicitly[List[Double]]
      result(1) should be (2.0)
    }


    it(
      """can be used for something like what Ruby has called
        |  monkey patching or Groovy calls mopping where we can add functionality to
        |  a class that we don't have access to, like isOdd/isEven
        |  in the Int class.  This is what we call implicit wrappers.
        |  First we will use a conversion method.""".stripMargin) {

      class IntWrapper(x:Int) {
        def isOdd: Boolean = x % 2 != 0
        def isEven: Boolean = !isOdd
      }

      import scala.language.implicitConversions

      implicit def int2IntWrapper(x:Int):IntWrapper = new IntWrapper(x)

      10.isOdd should be (false)
      10.isEven should be (true)
    }


    it( """Implicit wrappers can be created using a function and is often easier to mental map.""".stripMargin) {
      class IntWrapper(x:Int) {
        def isOdd: Boolean = x % 2 != 0
        def isEven: Boolean = !isOdd
      }

      import scala.language.implicitConversions

      implicit def int2IntWrapper: Int => IntWrapper = (x:Int) => new IntWrapper(x)

      10.isOdd should be (false)
      10.isEven should be (true)
    }

    it(
      """can be use a short hand version of this called implicit classes, before using them
        |  there are some rules:
        |  1. They can only be used inside of an object/trait/class
        |  2. They can only take one parameter in the constructor
        |  3. There can not be any colliding method name as that
        |     with the implicit outer scope""".stripMargin) {

      import scala.language.implicitConversions
      implicit class IntWrapper(x:Int) {
        def isOdd: Boolean = x % 2 != 0
        def isEven: Boolean = !isOdd
      }

      10.isOdd should be (false)
      10.isEven should be (true)
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a method""".stripMargin) {

      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(value:Int) extends Currency
      case class Yuan(value:Int) extends Currency
      case class Euro(value:Int) extends Currency

      def combine(x:Dollar, y:Dollar):Dollar = Dollar(x.value + y.value)

      implicit def int2Dollar(i:Int):Dollar = Dollar(i)

      combine(Dollar(100), Dollar(200)) should be (Dollar(300))
      combine(100, 200) should be (Dollar(300))
    }

    it(
      """can also convert things to make it fit into a particular API,
        | this is called implicit conversion,
        | in this scenario we will use a function""".stripMargin) {

      import scala.language.implicitConversions

      sealed abstract class Currency
      case class Dollar(value:Int) extends Currency
      case class Yuan(value:Int) extends Currency
      case class Euro(value:Int) extends Currency

      def combine(x:Dollar, y:Dollar):Dollar = Dollar(x.value + y.value)

      implicit val int2Dollar: Int => Dollar = (i:Int) => Dollar(i)

      combine(Dollar(100), Dollar(200)) should be (Dollar(300))
      combine(100, 200) should be (Dollar(300))

    }

    it(
      """is done automatically in Scala because what is inside of scala.Predef, for example,
        |  it explains how be can set a scala.Float , and there is java.lang.Float,
        |  java primitive float.
        |  We can investigate this by looking at
        |  the documentation.""".stripMargin) {

      val f: scala.Float = 3002.0f
      val f2: scala.Float = 30.0f

      val result = java.lang.Math.max(f, f2)
      result should be (3002.0f)
    }
  }

  describe("Locating implicits recipes") {
    it(
      """has a common way, to store that particular implicit
        |  recipe in an object that makes should make
        |  sense and then import that object""".stripMargin) {

      object MyPredef {
        import scala.language.implicitConversions
        implicit class IntWrapper(x:Int) {
          def isOdd: Boolean = x % 2 != 0
          def isEven: Boolean = !isOdd
        }
      }

      import MyPredef._
      10.isOdd should be (false)
    }

    it( """can also use a companion object to store any implicit recipes""".stripMargin) {
      def playPerformer(a:Artist) = s"Playing performer ${a.firstName} ${a.lastName}"

      playPerformer("Weird Al" -> "Yankovic") should be ("Playing performer Weird Al Yankovic")
    }

    it( """can also use a package object to store some of these implicits""") {
      def concatAll(xs:List[String]) = xs.reduce(_ + _)

      concatAll(3, "Hello") should be ("HelloHelloHello")
      concatAll((3, "Hello")) should be ("HelloHelloHello")
      concatAll(3 -> "Hello") should be ("HelloHelloHello")
    }

    it(
      """can use JavaConverters to convert a collection in
        | Java to Scala and vice versa""".stripMargin) {
      import java.time._
      import scala.collection.JavaConverters._

      val result = ZoneId.getAvailableZoneIds.asScala.toList
        .filter(_.startsWith("Asia")).map(s => s.split("/").last).sorted.size

      result should be (98)
    }
  }

  describe("View Bounds are used to ensure that there is a particular recipe for a certain type") {
    it(
      """Uses <% inside of a parameterized type declaration to determine if there is a conversion available
        | then within you can treat an object as an object of that type. It is unorthodox, and has since been
        | deprecated.""".stripMargin) {

      import scala.language.implicitConversions

      class Employee(val firstName:String, val lastName:String)


      implicit def stringToEmployee(s:String): Employee = {
        val tokens = s.split(" ")
        tokens.toList match {
          case List(x) => new Employee(x, "Unknown")
          case List(x, y, _*) => new Employee(x, y)
        }
      }

      def hireEmployee[A <% Employee](e:A) =
        s"Hired Employee ${e.firstName} ${e.lastName}"

      hireEmployee("Joe Armstrong") should be ("Hired Employee Joe Armstrong")
    }
  }

  describe(
    """Context Bounds works so that there is a type A, and it requires a B[A] somewhere
      |  within the the implicit scope, for example like Ordered[T], or TypeTag[T], or Numeric[T],
      |  this provides a way to check that something is something can be implicitly defined but
      |  gives the end user no opportunity to the ability to inject a different implementation""".stripMargin) {

    it(
      """uses the signature [T:WrappedType], which is
        | equivalent to (t:T)(implicit w:WrappedType[T])
        | let's try it with """.stripMargin) {

      trait Loggable[T] {
        def log(t:T):String
      }

      class Employee(val firstName:String, val lastName:String)

      object MyEmployeePredef {
        implicit val lastThenFirst: Loggable[Employee] = new Loggable[Employee] {
          override def log(t: Employee): String =
            s"Employee: ${t.lastName}, ${t.firstName}"
        }

        implicit val firstThenLast: Loggable[Employee] = new Loggable[Employee] {
          override def log(t: Employee): String =
            s"Employee: ${t.firstName} ${t.lastName}"
        }
      }

      import MyEmployeePredef.lastThenFirst

      def writeItOut[A:Loggable](a:A) = {
        val loggable = implicitly[Loggable[A]]
        loggable.log(a)
      }

      writeItOut(new Employee("Carly", "Simon")) should be ("Employee: Simon, Carly")
    }
  }

  describe(
    """Type Constraints are used to ensure that a particular method can run
      | if a particular generic is of a certain type, this is typically used for
      | one method""".stripMargin) {

    it(
      """uses one operator, =:= which is actually the full type =:=[A,B] that
        |  will to see if something is of the same type""".stripMargin) {

      class Pair[A,B](val a:A, val b:B) {
        def first:A = a
        def second:B = b
        def toList(implicit ev: A =:= B):List[A] = List(a,b).asInstanceOf[List[A]]
      }

      //val myPair = new Pair(10, "Foo")
      //myPair.toList
    }

    it("""uses the operator, <:< which will test if A is a subtype of B""") {
      pending
    }
  }

  describe("Getting around Erasure Using TypeTags") {
    it("used to use Manifest but now uses a type tag to retrieve what is erased") {

      import scala.reflect.runtime.universe._
      def matchList[A](list:List[A])(implicit tt:TypeTag[A]): String = {
        tt.tpe match {
          case x if x =:= typeOf[String] => "List of String"
          case y if y =:= typeOf[Int] => "List of Int"
          case _ => "Unknown"
        }
      }

      matchList(List(1,2,3,4)) should be ("List of Int")
    }
  }

  describe(
    """Typeclasses are a way of generating or extending behavior using Java-like interfaces,
      |  but operate as outside.  There is another term for this,
      |  and it's called ad-hoc polymorphism""".stripMargin) {

    it(
      """can be used to determine equality, so whether than make equals inside of an class,
        | it is now an outside concern""".stripMargin) {

      case class Team(city:String, mascot:String)

      trait Eq[A] {
        def isEqual(a:A, b:A):Boolean
      }

      object MyPredef2 {
        implicit val teamEqualsByCity: Eq[Team] = new Eq[Team] {
          override def isEqual(a: Team, b: Team): Boolean = a.city == b.city
        }

        implicit val teamEqualsByMascot: Eq[Team] = new Eq[Team] {
          override def isEqual(a: Team, b: Team): Boolean = a.mascot == b.mascot
        }

        implicit val teamEqualsByCityAndMascot: Eq[Team] = new Eq[Team] {
          override def isEqual(a: Team, b: Team): Boolean =
            teamEqualsByCity.isEqual(a,b) && teamEqualsByMascot.isEqual(a,b)
        }
      }

      import MyPredef2.teamEqualsByCity

      def equals[A](a:A, b:A)(implicit eqtc:Eq[A]): Boolean = eqtc.isEqual(a,b)

      def equals2[A:Eq](a:A, b:A): Boolean = {
        implicitly[Eq[A]].isEqual(a,b)
      }

      equals(Team("Cincinnati", "Bengals"), Team("Cincinnati", "Bengals")) should be (true)
      equals2(Team("Cincinnati", "Bengals"), Team("Cincinnati", "Bengals")) should be (true)
    }

    it("can be used for ordering") {
      case class Team(city:String, mascot:String)

      object MyPredef {
        implicit val orderTeamsByCityAsc: Ordering[Team] = new Ordering[Team]() {
          override def compare(x: Team, y: Team): Int = x.city.compareTo(y.city)
        }

        implicit val orderTeamsByMascotAsc: Ordering[Team] = new Ordering[Team]() {
          override def compare(x: Team, y: Team): Int = x.mascot.compareTo(y.mascot)
        }
      }

      val teams = List(Team("Cincinnati", "Bengals"),
                       Team("Madrid", "Real Madrid"),
                       Team("Las Vegas", "Golden Knights"),
                       Team("Houston", "Astros"),
                       Team("Cleveland", "Cavaliers"),
                       Team("Arizona", "Diamondbacks"))

      import MyPredef.orderTeamsByCityAsc

      teams.sorted.head.city should be ("Arizona")
      teams.min.city should be ("Arizona")
    }
  }
}
