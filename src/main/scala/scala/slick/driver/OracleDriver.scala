package scala.slick.driver

import java.util.UUID
import scala.slick.lifted._
import scala.slick.jdbc.{PositionedParameters, PositionedResult, JdbcType}
import scala.slick.ast.{SequenceNode, Library, FieldSymbol, Node}
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.compiler.CompilerState

import java.sql.{Types => SQLTypes}
/**
 * Slick driver for OracleSQL.
 *
 * This driver implements all capabilities of the
 * [[scala.slick.driver.ExtendedProfile]].
 *
 * Notes:
 *
 * <ul>
 *   <li>[[scala.slick.profile.SqlProfile.capabilities.typeBlob]]:
 *   The default implementation of the <code>Blob</code> type uses the
 *   database type <code>lo</code> and the stored procedure
 *   <code>lo_manage</code>, both of which are provided by the "lo"
 *   extension in OracleSQL.</li>
 * </ul>
 * http://db.apache.org/ddlutils/databases/oracle.html
 * https://github.com/hibernate/hibernate-orm/tree/master/hibernate-core/src/main/java/org/hibernate/dialect
 *
 * Status: unstable
 *
 */
trait OracleDriver extends JdbcDriver { driver =>

  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case SQLTypes.ARRAY => "lo" // todo why not "blob"?
    case SQLTypes.BIGINT => "NUMBER(38)"
    case SQLTypes.BINARY => "lo" // todo RAW
    case SQLTypes.BIT => "NUMBER(1)"
    case SQLTypes.BLOB => "lo"
    case SQLTypes.BOOLEAN  => "NUMBER(1)"
    case SQLTypes.DATALINK  => "lo"
    case SQLTypes.DECIMAL  => "NUMBER"
    case SQLTypes.DOUBLE => "DOUBLE PRECISION"
    case SQLTypes.NUMERIC  => "NUMBER"
    case SQLTypes.TIME => "DATE"
    case SQLTypes.TINYINT => "NUMBER(3)"
    case SQLTypes.VARCHAR => "VARCHAR2(255 BYTE)"
    case _ => super.defaultSqlTypeName(tmd)
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val concatOperator = Some("||")

    // http://stackoverflow.com/questions/2912144/alternatives-to-limit-and-offset-for-paging-in-oracle
    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b" limit $t offset $d"
      case (Some(t), None   ) => b" limit $t"
      case (None,    Some(d)) => b" offset $d"
      case _ =>
    }

    def limitString(sql: String, hasOffset: Boolean): String = {
       var sql_ = sql.trim()

      var isForUpdate = false;
      if ( sql.toLowerCase().endsWith(" for update") ) {
        sql_ = sql_.substring( 0, sql.length()-11 )
        isForUpdate = true
      }

      val pagingSelect: StringBuilder = new StringBuilder( sql.length()+100 )
      if (hasOffset) {
        pagingSelect.append("select * from ( select row_.*, rownum rownum_ from ( ")
      }
      else {
        pagingSelect.append("select * from ( ")
      }
      pagingSelect.append(sql)
      if (hasOffset) {
        pagingSelect.append(" ) row_ ) where rownum_ <= ? and rownum_ > ?")
      }
      else {
        pagingSelect.append(" ) where rownum <= ?")
      }

      if (isForUpdate ) {
        pagingSelect.append( " for update" )
      }

      return pagingSelect.toString()
    }

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Library.NextValue(SequenceNode(name)) => b"$name.nextval"
      case Library.CurrentValue(SequenceNode(name)) => b"$name.currval"
      case _ => super.expr(n, skipParens)
    }

    def selectClauseNullString(tmd: JdbcType[_]): String = tmd.sqlType match {
      case SQLTypes.VARCHAR | SQLTypes.CHAR => "to_char(null)"
      case SQLTypes.DATE | SQLTypes.TIMESTAMP | SQLTypes.TIME => "to_date(null)"
      case _ => "to_number(null)"
    }

    def currentTimestampSelectString(): String = {
      return "select sysdate from dual"
    }

    def currentTimestampSQLFunctionName(): String =  {
      return "sysdate"
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
   /* override protected def createIndex(idx: Index) = {
      if(idx.unique) {
        /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
         * index) because Derby does not allow a FOREIGN KEY CONSTRAINT to
         * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
         * CONSTRAINT. */
        val sb = new StringBuilder append "ALTER TABLE " append quoteIdentifier(table.tableName) append " ADD "
        sb append "CONSTRAINT " append quoteIdentifier(idx.name) append " UNIQUE("
        addIndexColumnList(idx.on, sb, idx.table.tableName)
        sb append ")"
        sb.toString
      } else super.createIndex(idx)
    } */

    override def createPhase1 = super.createPhase1 ++ columns.flatMap {
      case cb: ColumnDDLBuilder => cb.createLobTrigger(table.tableName)
    }
    //test2: create table "a" ("id" INTEGER NOT NULL,"a" VARCHAR2 DEFAULT 'foo' NOT NULL,"b" NUMBER(1) DEFAULT true)
    //> create table "a" ("id" INTEGER NOT NULL,"a" VARCHAR2(255 BYTE) DEFAULT 'foo' NOT NULL,"b" NUMBER(1) DEFAULT 1)
     // CONSTRAINT supplier_pk PRIMARY KEY (supplier_id, supplier_name)

    override def dropPhase1 = {
      val dropLobs = columns.flatMap {
        case cb: ColumnDDLBuilder => cb.dropLobTrigger(table.tableName)
      }
      if(dropLobs.isEmpty) super.dropPhase1
      else Seq("delete from "+quoteIdentifier(table.tableName)) ++ dropLobs ++ super.dropPhase1
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder) {
      sb append quoteIdentifier(column.name) append ' '
      if(autoIncrement) {
        autoIncrement = false
      }
      else sb append sqlType
      appendOptions(sb)
    }

    def lobTrigger(tname: String) =
      quoteIdentifier(tname+"__"+quoteIdentifier(column.name)+"_lob")

    def createLobTrigger(tname: String): Option[String] =
      if(sqlType == "lo") Some(
        "create trigger "+lobTrigger(tname)+" before update or delete on "+
        quoteIdentifier(tname)+" for each row execute procedure lo_manage("+quoteIdentifier(column.name)+")"
      ) else None

    def dropLobTrigger(tname: String): Option[String] =
      if(sqlType == "lo") Some(
        "drop trigger "+lobTrigger(tname)
      ) else None
  }

 class SequenceDDLBuilder[T](seq: Sequence[T]) extends super.SequenceDDLBuilder(seq) {
    override def buildDDL: DDL = {
      import seq.integral._
      val increment = seq._increment.getOrElse(one)
      val desc = increment < zero
      val b = new StringBuilder append "CREATE SEQUENCE " append quoteIdentifier(seq.name)
      /* Set the START value explicitly because it defaults to the data type's
       * min/max value instead of the more conventional 1/-1. */
      b append " START WITH " append seq._start.getOrElse(if(desc) -1 else 1)
      seq._increment.foreach { b append " INCREMENT BY " append _ }
      seq._maxValue.foreach { b append " MAXVALUE " append _ }
      seq._minValue.foreach { b append " MINVALUE " append _ }
      /* Cycling is supported but does not conform to SQL:2008 semantics. Derby
       * cycles back to the START value instead of MINVALUE or MAXVALUE. No good
       * workaround available AFAICT. */
      if(seq._cycle) b append " CYCLE"
      DDL(b.toString, "DROP SEQUENCE " + quoteIdentifier(seq.name))
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val byteArrayJdbcType = new ByteArrayJdbcType
    override val uuidJdbcType = new UUIDJdbcType

    class ByteArrayJdbcType extends super.ByteArrayJdbcType {
      override val sqlType = SQLTypes.BLOB
      override val sqlTypeName = "BLOB"
      override def setOption(v: Option[Array[Byte]], p: PositionedParameters) = v match {
        case Some(a) => p.setBytes(a)
        case None => p.setNull(sqlType)
      }
    }

    // http://stackoverflow.com/questions/7690980/oracle-cannot-lookup-row-in-database-by-uuid
    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlTypeName = "UUID"
      override def setValue(v: UUID, p: PositionedParameters) = p.setObject(v, sqlType)
      override def setOption(v: Option[UUID], p: PositionedParameters) = p.setObjectOption(v, sqlType)
      override def nextValue(r: PositionedResult) = r.nextObject().asInstanceOf[UUID]
      override def updateValue(v: UUID, r: PositionedResult) = r.updateObject(v)
      override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
    }
  }
}

object OracleDriver extends OracleDriver
