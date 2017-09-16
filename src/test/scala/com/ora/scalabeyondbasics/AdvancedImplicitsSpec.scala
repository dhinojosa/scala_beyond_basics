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

       implicit val rate = 100  //Int, 100

       //100s lines of code

       def calculatePayment(hours:Int)(implicit rate:Int) = hours * rate

       calculatePayment(50) should be (5000)
    }

    it("""will allow you to place something manually, if you want to override the implicit value""".stripMargin) {

      implicit val rate = 100  //Int, 100

      //100s lines of code

      def calculatePayment(hours:Int)(implicit rate:Int) = hours * rate

      calculatePayment(50)(150) should be (7500)
    }

    it(
      """will gripe at compile time if there are two implicit bindings of the same type.  It's
        |  worth noting that what Scala doing are compile time tricks for implicit. One strategy is to
        |  wrap a value in a type to avoid conflict""".stripMargin) {

      case class Rate(value:Int)
      case class Age(value:Int)

      implicit val rate = Rate(100)  //Rate, 100
      implicit val age = Age(40)    //Age, 40

      //100s lines of code

      def calculatePayment(hours:Int)(implicit rate:Rate) = {
        rate match {
          case Rate(x) => hours * x
          case _ => 0
        }
      }

      calculatePayment(50) should be (5000)
    }


    it(
      """is really used to bind services that require something and
        |  you don't particularly need to inject everywhere explicitly, in this
        |  case let's discuss Future[+T]""".stripMargin) {

      import scala.concurrent._
      import java.util.concurrent.Executors

      implicit val executionContext: ExecutionContext =
        ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

      val f = Future {
        println("Thread is: " + Thread.currentThread().getName)
        Thread.sleep(4000)
        100 + 50
      }

      f.foreach(x => println(x))
    }


    it( """can bring up any implicit directly by merely calling up implicitly""") {
      case class IceCream(name:String)
      case class Scoops(num:Int, flavor:IceCream)
      implicit val flavorOfTheDay = IceCream("Holiday Cheer")

      def orderIceCream(num:Int):Scoops = {
        Scoops(num, implicitly[IceCream])
      }

      orderIceCream(3) should be (Scoops(3, IceCream("Holiday Cheer")))
    }

    it(
      """the implicit group parameter list, can contain more than one paramter, but
        |  needs to be in the same implicit parameter group""".stripMargin) {

      implicit val tunaSandwich = 5000  //Int, 5000
      implicit val fritoChips = "Euro"  //String, "Euro"

      def calculateYearRate(amount:Int)(implicit bonus:Int, currency:String):String = {
         amount + bonus + " " + currency
      }

      calculateYearRate(60000) should be ("65000 Euro")
    }

    it( """can also be replaced with default parameters, choose accordingly""") {

      def calculateYearRate(amount:Int, bonus:Int = 5000, currency:String = "Euro"):String = {
        amount + bonus + " " + currency
      }

      calculateYearRate(60000) should be ("65000 Euro")
    }

    it(
      """can be used for something like what Ruby has called
        |  monkey patching or Groovy calls mopping where we can add functionality to
        |  a class that we don't have access to, like isOdd/isEven
        |  in the Int class.  This is what we call implicit wrappers.
        |  First we will use a conversion method.""".stripMargin) {
  
       import scala.language.implicitConversions
       
       class IntWrapper(x:Int) {
         def isOdd:Boolean = x % 2 != 0
         def isEven:Boolean = !isOdd
       }
       
       //Within the scope, the an implicit entry: (Int) => IntWrapper | new Wrapper(x)
       
       implicit def iHopeAliensDontDestroyUs(x:Int):IntWrapper = new IntWrapper(x)
       
       45.isEven should be (false)
       45.isOdd should be (true)
    }


    it( """Implicit wrappers can be created using a function and is often easier to mental map.""".stripMargin) {
      import scala.language.implicitConversions
       
       class IntWrapper(x:Int) {
         def isOdd:Boolean = x % 2 != 0
         def isEven:Boolean = !isOdd
       }
       
       //Within the scope, the an implicit entry: (Int) => IntWrapper | new Wrapper(x)
       
       implicit val iHopeAliensDontDestroyUs = (x:Int) => new IntWrapper(x)
       
       45.isEven should be (false)
       45.isOdd should be (true)
    }


    it("""can be use a short hand version of this called implicit classes, before using them
        |  there are some rules:
        |  1. They can only be used inside of an object/trait/class
        |  2. They can only take one parameter in the constructor
        |  3. There can not be any colliding method name as that with the implicit outer scope""".stripMargin) {

       import scala.language.implicitConversions
       
       implicit class IntWrapper(x:Int) {
         def isOdd:Boolean = x % 2 != 0
         def isEven:Boolean = !isOdd
       }
       
       45.isEven should be (false)
       45.isOdd should be (true)
    }

    it("""can also convert things to make it fit into a particular API, this is called implicit conversion,
        | in this scenario we will use a method""".stripMargin) {

      sealed abstract class Currency
      case class Dollar(value:Int) extends Currency
      case class Yen(value: Int) extends Currency
      
      import scala.language.implicitConversions
      
      implicit def int2Dollar(x:Int) = Dollar(x)
      
      def addAmounts(x:Dollar, y:Dollar) = Dollar(x.value + y.value)
      
      addAmounts(100, 200) should be (Dollar(300))
    }

    it("""can also convert things to make it fit into a particular API, this is called implicit conversion,
        | in this scenario we will use a function""".stripMargin) {
      sealed abstract class Currency
      case class Dollar(value:Int) extends Currency
      case class Yen(value: Int) extends Currency
      
      import scala.language.implicitConversions
      
      implicit val int2Dollar = (x:Int) => Dollar(x)
      
      def addAmounts(x:Dollar, y:Dollar) = Dollar(x.value + y.value)
      
      addAmounts(100, 200) should be (Dollar(300))
    }

    it( """is done automatically in Scala because what is inside of scala.Predef, for example,
        |  it explains how be can set a scala.Float , and there is java.lang.Float, java primitive float.
        |  We can investigate this by looking at
        |  the documentation.""".stripMargin) {
      val a: scala.Float = 3000.1f
      val b: scala.Float = 4000.2f
      
      val result = java.lang.Math.min(a, b)
    }
  }

  describe("Locating implicits recipes") {
    it( """has a common way, to store that particular implicit
        |  recipe in an object that makes should make
        |  sense and then import that object""".stripMargin) {
       object MyPredef {
         implicit class IntWrapper(x:Int) {
           def isOdd:Boolean = x % 2 != 0
           def isEven:Boolean = !isOdd
         }
       }

       import MyPredef._
       11.isOdd should be (true)
    }

    it( """can also use a companion object to store any implicit recipes""".stripMargin) {
       class Artist(val firstName:String, val lastName:String) {
         override def toString: String = s"$firstName $lastName"
       }
       object Artist {
         import scala.language.implicitConversions

         implicit def tupleToEmployee(t:(String, String)):Artist = new Artist(t._1, t._2)
       }

       def playArtist(a:Artist) = s"Artist $a is now playing"

       import Artist._
       playArtist("Stevie" -> "Wonder") should be ("Artist Stevie Wonder is now playing")
    }

    it( """can also use a package object to store some of these implicits""") {
        def numItems(list:List[String]) = list.reduce(_ + _)
        
        numItems(3 -> "Whoa") should be ("WhoaWhoaWhoa")
    }

    it("""can use JavaConverters to convert a collection in Java to Scala and vice versa""") {
       
      import scala.collection.JavaConverters._ //implicits 
      import java.time.ZoneId
      
      ZoneId.getAvailableZoneIds.asScala.toSet
        .filter(x => x.startsWith("America"))
        .map(x => x.split("/")(1)).toList.sorted
    }
  }

  describe("View Bounds are used to ensure that there is a particular recipe for a certain type") {
    it("""Uses <% inside of a parameterized type declaration to determine if there is a conversion available
        | then within you can treat an object as an object of that type. It is unorthodox, and has since been
        | deprecated.""".stripMargin) {

      class Employee(val firstName:String, val lastName:String)

      import scala.language.implicitConversions

      implicit def str2Employee(s:String):Employee = {
        s.split(" ").toList match {
          case Nil => new Employee("John", "Anonymous")
          case fn :: Nil => new Employee(fn, "Anonymous")
          case fn :: ln :: Nil => new Employee(fn, ln)
          case fn :: ln :: _ => new Employee(fn, ln)
        }
      }

      def hireEmployee[A <% Employee](a:A):String = {
         s"Hired ${a.firstName} ${a.lastName}"
      }

      hireEmployee("Bob Henderson") should be ("Hired Bob Henderson")
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
        def log(t:T):String //abstract
      }

      class Employee(val firstName:String, val lastName:String)

      implicit val loggableEmployee: Loggable[Employee] = new Loggable[Employee] {
        override def log(t: Employee):String = s"${t.firstName} ${t.lastName}"
      }

      def toStringz[T:Loggable](t:T): String = {
        val loggable = implicitly[Loggable[T]]
        loggable.log(t)
      }

      toStringz(new Employee("Brazil", "Thailand")) should be ("Brazil Thailand")
    }
  }

  describe(
    """Type Constraints are used to ensure that a particular method can run
      | if a particular generic is of a certain type, this is typically used for
      | one method""".stripMargin) {
    it(
      """uses one operator, =:= which is actually the full type =:=[A,B] that
        |  will to see if something is of the same type""".stripMargin) {


      class MyPair[A, B](val a:A, val b:B) {
          def first: A = a
          def second: B = b
          def toList(implicit x:A =:= B):List[A] = List(a,b).asInstanceOf[List[A]]
      }


      val myPair = new MyPair(4, 10)
      myPair.toList should be (List(4, 10))
    }

    it("""uses the operator, <:< which will test if A is a subtype of B""") {
       val list = List((1, "One"), (2, "Two"), (3, "Three"))
       list.toMap.get(1) should be (Some("One"))
    }
  }

  describe("Getting around Erasure Using TypeTags") {

    it("used to use Manifest but now uses a type tag to retrieve what is erased") {
      import scala.reflect.runtime.universe._

      def matchList[A](list:List[A])(implicit tt:TypeTag[A]):String = {
        tt.tpe match {
          case t if t =:= typeOf[Int]    => "List of Int"
          case t if t =:= typeOf[String] => "List of String"
          case _                         => "List of Unknowns"
        }
      }

      matchList(List("Foo", "Bar", "Baz")) should be ("List of String")
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
        override def equals(a: Employee, b: Employee): Boolean = a.lastName == b.lastName &&
                                                                 a.firstName == b.firstName
      }

      def equalz[A](a:A, b:A)(implicit eq:Eq[A]) = eq.equals(a, b)

      equalz(new Employee("Stan", "Lee"), new Employee("Stan", "Lee")) should be (true)
    }

    it("can be used for ordering") {
      class Employee(val firstName:String, val lastName:String)
      object Employee {
        implicit val orderingEmployeeLastName = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int = {
            x.lastName.compareTo(y.lastName)
          }
        }

        implicit val orderingEmployeeFirstName = new Ordering[Employee] {
          override def compare(x: Employee, y: Employee): Int = {
            x.firstName.compareTo(y.firstName)
          }
        }
      }

      import Employee.orderingEmployeeFirstName

      val sortedList = List(new Employee("Ascunsion", "Bolivia"),
                            new Employee("Tokyo", "Japan"),
                            new Employee("Addis Ababa", "Ethiopia"),
                            new Employee("Columbo", "Sri Lanka"))
                       .sorted


      sortedList.apply(3).lastName should be ("Japan")
    }
  }
}
