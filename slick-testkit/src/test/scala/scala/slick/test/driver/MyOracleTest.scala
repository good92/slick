package slick.test.driver

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{TestDB, Testkit, ExternalTestDB, DriverTest}
import scala.slick.driver.{JdbcProfile, OracleDriver}
import scala.slick.jdbc.ResultSetInvoker
// import scala.slick.session.Session           - deprecated


@RunWith(classOf[Testkit])
class MyPostgresTest extends DriverTest(MyOracleTest.tdb)

object MyOracleTest {
  // JdbcProfile.Backend#Session

  def tdb(cname: String) = new ExternalTestDB("mypostgres", OracleDriver) /*{
    override def getLocalTables(implicit session: profile.Backend#Session) = {
      val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
      tables.list.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
    }
    override def getLocalSequences(implicit session: profile.Backend#Session) = {
      val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
      tables.list.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
    override lazy val capabilities = driver.capabilities + TestDB.plainSql
  }      */
}
