package com.typesafe.slick.testkit.util

import scala.language.existentials
import org.junit.runner.Description
import org.junit.runner.notification.RunNotifier
import org.junit.runners.model._
import org.junit.Assert._
import scala.slick.profile.{SqlProfile, Capability}
import scala.slick.driver.JdbcProfile
import com.typesafe.slick.testkit.{tests => tk}
import java.lang.reflect.Method
import scala.slick.driver.JdbcDriver.Table
import scala.slick.jdbc.UnitInvoker
import scala.slick.lifted.DDL

/** Lists all tests of the Slick driver test kit */
object Testkit {
  val tests: List[Class[_ <: TestkitTest]] =
    classOf[tk.AggregateTest] ::
    classOf[tk.ColumnDefaultTest] ::
    classOf[tk.CountTest] ::
    classOf[tk.DataTypeTest] ::
    classOf[tk.ExecutorTest] ::
    classOf[tk.ForeignKeyTest] ::
    classOf[tk.InsertTest] ::
    classOf[tk.InvokerTest] ::
    classOf[tk.IterateeTest] ::
    classOf[tk.JoinTest] ::
    classOf[tk.MainTest] ::
    classOf[tk.MapperTest] ::
    classOf[tk.MiscTest] ::
    classOf[tk.MutateTest] ::
    classOf[tk.NestingTest] ::
    classOf[tk.NewQuerySemanticsTest] ::
    classOf[tk.PagingTest] ::
    classOf[tk.PlainSQLTest] ::
    classOf[tk.PrimaryKeyTest] ::
    classOf[tk.ScalarFunctionTest] ::
    classOf[tk.SequenceTest] ::
    classOf[tk.TemplateTest] ::
    classOf[tk.TransactionTest] ::
    classOf[tk.UnionTest] ::
    (Nil: List[Class[_ <: TestkitTest]])
}

/** JUnit runner for the Slick driver test kit. */
class Testkit(clazz: Class[_ <: DriverTest], runnerBuilder: RunnerBuilder) extends SimpleParentRunner[TestMethod](clazz) {

  val driverTest = clazz.newInstance
  var tdb: TestDB = driverTest.tdbSpec(clazz.getName)

  def describeChild(ch: TestMethod) = ch.desc

  def getChildren = if(tdb.isEnabled) {
    driverTest.tests.flatMap { t =>
      val ms = t.getMethods.filter { m =>
        m.getName.startsWith("test") && m.getParameterTypes.length == 0
      }
      ms.map { m =>
        val tname = m.getName + '[' + tdb.confName + ']'
        new TestMethod(tname, Description.createTestDescription(t, tname), m, t)
      }
    }
  } else Nil

  override def runChildren(notifier: RunNotifier) = if(!children.isEmpty) {
    tdb.cleanUpBefore()
    try {
      val is = children.iterator.zipWithIndex.toIndexedSeq
      val last = is.length - 1
      var previousTestObject: TestkitTest = null
      for((ch, idx) <- is) {
        val desc = describeChild(ch)
        notifier.fireTestStarted(desc)
        try {
          val testObject =
            if(previousTestObject ne null) previousTestObject
            else ch.cl.getConstructor(classOf[TestDB]).newInstance(tdb)
          previousTestObject = null
          try {
            ch.method.invoke(testObject)
          } finally {
            val skipCleanup = idx == last || (testObject.reuseInstance && (ch.cl eq is(idx+1)._1.cl))
            if(skipCleanup) {
              if(idx == last) testObject.closeKeepAlive()
              else previousTestObject = testObject
            }
            else testObject.cleanup()
          }
        } catch {
          case t: Throwable => addFailure(t, notifier, desc)
        } finally notifier.fireTestFinished(desc)
      }
    } finally tdb.cleanUpAfter()
  }
}

abstract class DriverTest(val tdbSpec: TestDB.TestDBSpec) {
  def tests = Testkit.tests
}

case class TestMethod(name: String, desc: Description, method: Method, cl: Class[_ <: TestkitTest])

trait TestkitTest {
  val tdb: TestDB
  private[this] var keepAliveSession: tdb.profile.Backend#Session = null

  protected implicit def sharedSession: tdb.profile.Backend#Session = {
    db
    keepAliveSession
  }

  val reuseInstance = false

  lazy val db = {
    val db = tdb.createDB()
    keepAliveSession = db.createSession()
    if(!tdb.isPersistent && tdb.isShared)
      keepAliveSession.conn // keep the database in memory with an extra connection
    db
  }

  def cleanup() = if(keepAliveSession ne null) {
    try if(tdb.isPersistent) tdb.dropUserArtifacts(keepAliveSession)
    finally closeKeepAlive()
  }

  def closeKeepAlive() = {
    if(keepAliveSession ne null) keepAliveSession.close()
  }

  def assertFail(f: =>Unit) = {
    var succeeded = false
    try {
      f
      succeeded = true
    } catch {
      case e: Exception if !scala.util.control.Exception.shouldRethrow(e) =>
    }
    if(succeeded) fail("Exception expected")
  }

  def assertAllMatch[T](t: TraversableOnce[T])(f: PartialFunction[T, _]) = t.foreach { x =>
    if(!f.isDefinedAt(x)) fail("Expected shape not matched by: "+x)
  }

  def scap = SqlProfile.capabilities
  def jcap = JdbcProfile.capabilities
  def ifCap[T](caps: Capability*)(f: => T): Unit =
    if(caps.forall(c => tdb.capabilities.contains(c))) f
  def ifNotCap[T](caps: Capability*)(f: => T): Unit =
    if(!caps.forall(c => tdb.capabilities.contains(c))) f

  // http://stackoverflow.com/questions/1193333/using-either-to-process-failures-in-scala-code
  // http://stackoverflow.com/questions/5329078/get-arguments-back-from-partially-applied-function-in-scala
  def throwableToLeft[T](action: => T): Either[java.lang.Throwable, T] =
    try {
      Right(action)
    } catch {
      case ex: Throwable => Left(ex)
  }

  // http://stackoverflow.com/questions/8556743/printing-out-a-functions-name-in-println  -> todo: print ddl.statements or query
  // http://oldfashionedsoftware.com/2008/08/23/fun-with-scala-functions/
  def analyze[A](function: => A) {
     function
  }

 /* def print[A](ddl: DDL) {
      ddl.createStatements foreach println
  } */

  def run[A, F](testFunction: => Boolean, actionFunction: => A, message: String, fixFunction: => F, /* var */ depthLimit: Int){
    println("run action")
    //i-- depthLimit-- // Fail with: 'value -- is not a member of Int' -> val/var problem
    // http://stackoverflow.com/questions/9535821/scala-mutable-var-method-parameter-reference
    val depth = depthLimit - 1

    throwableToLeft {
      println(actionFunction.toString)   // http://stackoverflow.com/questions/8556743/printing-out-a-functions-name-in-println  -> todo: print ddl.statements or query
      actionFunction
    } match {
      case Right(actionFunction) => println(message)
      case Left(actionFunction) =>
        println(actionFunction.printStackTrace)
        fixIt(testFunction, actionFunction, message, fixFunction, depth)
    }
  }

  // http://stackoverflow.com/questions/9822149/scala-boolean-function-abstraction
  def fixIt[A, F](testFunction: => Boolean, actionFunction: => A, message: String, fixFunction: => F, /* var */ depthLimit: Int){
    val depth = depthLimit
    println("before fix")
   try {
     println(fixFunction.toString)
     fixFunction
   } catch { case ex: Throwable => }
    println("after fix")
    run(testFunction, actionFunction, message, fixFunction, depth)
  }

  // http://stackoverflow.com/questions/6349202/can-i-pass-an-arbitrary-function-to-another-function-in-scala
  def logOrFix[A, F](testFunction: => Boolean, actionFunction: => A, message: String, fixFunction: => F, /* var */ depthLimit: Int){
    if (depthLimit==0) return

    val depth = depthLimit

    if (!testFunction) {
      fixIt(testFunction, actionFunction, message, fixFunction, depth)
      return
    }

    run(testFunction, actionFunction, message, fixFunction, depth)
  }

  def log[T](action: => T, message: String){
    throwableToLeft { action } match {
      case Right(s) => println(message)
      case Left(e) => println("FAIL:" + message); throw e
    }
  }

  def logOrFixCreation[A, F](testFunction: => Boolean, actionFunction: => A, fixFunction: => F){
    logOrFix(testFunction, actionFunction, "Table Creation done.", fixFunction, 2)
  }

  def logOrFixCreation(ddl: DDL){
    //tdb.driver.capabilities
    log(ddl)
    val i = tdb.driver.Implicit.ddlToDDLInvoker(ddl)

    logOrFix(i.tableExist(this.tdb.getLocalTables), i.create, "Table(s) Creation(s) done.", i.drop, 2)
  }

  def logOrFixCreation(tt: Any){//Table[_]){  // Table)
    //tdb.driver.capabilities
  val t = tt.asInstanceOf[Table[_]]
    log(t.ddl)

    val i = tdb.driver.Implicit.ddlToDDLInvoker(t.ddl)
    //val i = tdb.driver(t.ddl)

    logOrFix(i.tableExist(List(t.tableName)), i.create, "Table Creation done for " + t.tableName, i.drop, 2)
  }

  def logInsert[T](action: => T){
    log(action, "Insertion done.")
  }

  def logInsert(){
    println("Insertion done.")
  }

  def drop(t: Any){//Table[_]){
    val tt = t.asInstanceOf[Table[_]]
    drop(tt.ddl)
  }
  def drop(ddl: DDL){
    logDrop(ddl)
    val i = tdb.driver.Implicit.ddlToDDLInvoker(ddl)
    i.drop
  }

  def log(ddl: DDL){ddl.createStatements.foreach(println)}
  def logDrop(ddl: DDL){ddl.dropStatements.toVector.reverseIterator.foreach(println)}

  def log(t: Any){
    // tofix: java.lang.ClassCastException: scala.slick.lifted.DDL$$anon$2 cannot be cast to scala.slick.driver.JdbcTableComponent$Table
    val tt = t.asInstanceOf[Table[_]]
    log(tt.ddl)
  }

  def logDrop(tt: Any){ // Table[_] - _ :< Table
    val t = tt.asInstanceOf[Table[_]]
    logDrop(t.ddl)
  }
}
