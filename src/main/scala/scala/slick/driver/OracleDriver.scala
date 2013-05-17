package scala.slick.driver

import java.util.UUID
import scala.slick.lifted._
import scala.slick.jdbc.{PositionedParameters, PositionedResult, JdbcType}
import slick.ast._
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.compiler.CompilerState

import java.sql.{Types => SQLTypes, DatabaseMetaData}
import scala._
import collection.mutable.ListBuffer
import scala.Some
import slick.ast._
import slick.ast.FieldSymbol
import slick.ast.SequenceNode
import slick.jdbc.meta.MForeignKey
import slick.profile.{SqlProfile, Capability}
import scala.Some
import scala.Some

/**
 * Slick driver for OracleSQL - tests on Oracle11G.
 *
 * This driver implements all capabilities of the
 * [[scala.slick.driver.JdbcProfile]].
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
 * http://wiki.postgresql.org/wiki/Oracle_to_Postgres_Conversion
 *
 * http://jen.fluxcapacitor.net/geek/autoincr.html
 *
 * Status: unstable: 21 success /37 failed   - ORACLE11G
 *
 */
trait OracleDriver extends JdbcDriver { driver =>
  /* override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcProfile.capabilities.returnInsertOther
    - SqlProfile.capabilities.sequenceLimited
    ) */


  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case SQLTypes.ARRAY => "BLOB"
    case SQLTypes.BIGINT => "NUMBER(19,0)"
    case SQLTypes.BINARY => "BLOB" // todo RAW
    case SQLTypes.BIT => "NUMBER(1,0)"
    case SQLTypes.BLOB => "BLOB"
    case SQLTypes.BOOLEAN  => "NUMBER(1)"
    case SQLTypes.DATALINK  => "BLOB"
    case SQLTypes.DECIMAL  => "NUMBER(10,0)"
    case SQLTypes.DOUBLE => "DOUBLE PRECISION"
    case SQLTypes.CHAR => "CHAR(1 CHAR)"
    case SQLTypes.DATE => "DATE"
    case SQLTypes.INTEGER => "NUMBER(10,0)"
    case SQLTypes.NUMERIC  => "NUMBER(10,0)"
    case SQLTypes.SMALLINT => "NUMBER(5,0)"
    case SQLTypes.TIME => "DATE"
    case SQLTypes.TIMESTAMP => "TIMESTAMP"
    case SQLTypes.TINYINT => "NUMBER(3,0)"
    case SQLTypes.VARCHAR => "VARCHAR2(255 BYTE)"
    case _ => super.defaultSqlTypeName(tmd)
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val concatOperator = Some("||")
    override protected val scalarFrom = Some("DUAL")

    // http://stackoverflow.com/questions/7480243/sql-oracle-order-by-and-limit
    // http://stackoverflow.com/questions/2912144/alternatives-to-limit-and-offset-for-paging-in-oracle
    // http://stackoverflow.com/questions/7326885/limit-offset-in-oracle-11g
    // https://github.com/mysema/querydsl/issues/140
    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = {
      // var sql_ = sql.trim()

      var isForUpdate = false
      if (b.toString.toLowerCase().endsWith(" for update") ) {
        (fetch, offset) match {
          case (Some(t), Some(d)) =>
            b =+ "select * from ( "
            b" ) where rownum_ <= $t and rownum_ > $d"
          case (Some(t), None   ) =>
              b =+ "select * from ( "
              b" ) where rownum <= $t"
          case (None,    Some(d)) =>
              b =+ "select * from ( "
              b" ) where  rownum_ > $d"
          case _ =>
        }
      } else {
        (fetch, offset) match {
          case (Some(t), Some(d)) =>
            b =+ "select * from ( select row_.*, rownum rownum_ from ( "
            b" ) row_ ) where rownum_ <= $t and rownum_ > $d"
          case (Some(t), None   ) =>
            b =+ "select * from ( select row_.*, rownum rownum_ from ( "
            b" ) row_ ) where rownum <= $t"
          case (None,    Some(d)) =>
            b =+ "select * from ( select row_.*, rownum rownum_ from ( "
            b" ) row_ ) where  rownum_ > $d"
          case _ =>
        }
      }
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
    private var countAutoIncrement = 0

   override protected def createIndex(idx: Index) = {
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
    }

    override def createPhase1 = super.createPhase1 ++ columns.flatMap {
      case cb: ColumnDDLBuilder => cb.createLobTrigger(table.tableName)
    }

    // autoIncr -> sequence + trigger
    override def createPhase2 = super.createPhase2 ++ createAutoIncSequencesAndTriggers()

    override def dropPhase2 = super.createPhase2 ++ createAutoIncSequencesAndTriggers()

    def initCountAutoIncrement {
      this.countAutoIncrement = 0
      // todo: columns.forall( column => column.autoIncrement) countAutoIncrement = countAutoIncrement + 1
      for (column <- columns) {
        if (column.autoIncrement)
          this.countAutoIncrement = this.countAutoIncrement + 1
      }
    }

   def createAutoIncSequencesAndTriggers(): Iterator[String] = {
      val lb = new ListBuffer[String]
      initCountAutoIncrement

      if (this.countAutoIncrement == 0) return lb.toIterator
      val lb2 = new ListBuffer[String]

     var name = table.tableName
       //  columns.forall( column => column.autoIncrement) match case n if =>
     for (column <- columns) {
       if (column.autoIncrement) {
         // column: FieldSymbol
         if (countAutoIncrement > 1) name = name + "_" + column.getName
         lb += "create sequence " + name + "_seq start with 1 increment by 1 nomaxvalue"

         if (countAutoIncrement == 1) {
           lb += "create trigger " + table.tableName + "_trigger before insert on " + table.tableName +
             " for each row begin select " + name + "_seq.nextval into :new." + column.getName + " from dual; end"
           return lb.toIterator
         } else {
           lb2 += "select " + name + "_seq.nextval into :new." + column.getName + " from dual;"
         }
       }
     }

    lb += "create trigger " + table.tableName + "_trigger before insert on " + table.tableName + " for each row begin "
    lb ++ lb2
    lb += "end;"
    lb.toIterator
  }

   def dropAutoIncTriggersAndSequences(): Iterable[String] = {
     val lb = new ListBuffer[String]
     initCountAutoIncrement

     if (this.countAutoIncrement == 0) return lb.toIterable
     val lb2 = new ListBuffer[String]

     var name = table.tableName
     lb += "drop trigger " + name + "_seq"
     //  columns.forall( column => column.autoIncrement) match case n if =>
     for (column <- columns.toList.reverse) {
       if (column.autoIncrement) {
         if (countAutoIncrement > 1) name = name + "_" + column.getName
         lb += "drop sequence " + name + "_seq"
       }
     }
     lb.toIterable
   }

    override def dropPhase1 = {
      val dropLobs = columns.flatMap {
        case cb: ColumnDDLBuilder => cb.dropLobTrigger(table.tableName)
      }
      if(dropLobs.isEmpty) {
        if (this.countAutoIncrement == 0) {
          super.dropPhase1
        } else {
          dropAutoIncTriggersAndSequences ++ super.dropPhase1
        }
      } else Seq("delete from "+quoteIdentifier(table.tableName)) ++ dropLobs ++ super.dropPhase1
    }

    override private[driver] def addForeignKey(fk: ForeignKey[_ <: TableNode, _], sb: StringBuilder) {
      sb append "constraint " append quoteIdentifier(fk.name) append " foreign key("
      addForeignKeyColumnList(fk.linearizedSourceColumns, sb, table.tableName)
      sb append ") references " append quoteIdentifier(fk.targetTable.tableName) append "("
      addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
      if (fk.onUpdate.action==ForeignKeyAction.NoAction) {
        sb append ")"
      } else {
        sb append ") on update " append fk.onUpdate.action
        sb append " on delete " append fk.onDelete.action
      }
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {

    override def appendColumn(sb: StringBuilder) {
      sb append quoteIdentifier(column.name) append ' '
     /* if(autoIncrement) {
        autoIncrement = false
      }*/
      sb append sqlType
      if (sqlType.isEmpty) sb append SQLTypes.BIGINT // id
      appendOptions(sb)
    }

    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) {
        if ((defaultLiteral == true) || (defaultLiteral == "TRUE")) sb append " DEFAULT 1"
        else if ((defaultLiteral == false) || (defaultLiteral == "FALSE"))  sb append " DEFAULT 0"
        else sb append " DEFAULT " append defaultLiteral
      }
    // todo: Auto create Sequence for the column
    //  if(autoIncrement) println("ERROR autoIncrement not supported for column:\n"+column.name
    //                           +". You must use Sequences:\n"+sb.toString)

      if(notNull) sb append " NOT NULL"
      else if(sqlType.toUpperCase == "TIMESTAMP") sb append " NULL"

      if(primaryKey) sb append " PRIMARY KEY"
    }

    def lobTrigger(tname: String) =
      quoteIdentifier(tname+"__"+quoteIdentifier(column.name)+"_lob")

    def createLobTrigger(tname: String): Option[String] =
      if(sqlType == "BLOB") Some(
        "create trigger "+lobTrigger(tname)+" before update or delete on "+
        quoteIdentifier(tname)+" for each row execute procedure lo_manage("+quoteIdentifier(column.name)+")"
      ) else None

    def dropLobTrigger(tname: String): Option[String] =
      if(sqlType == "BLOB") Some(
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

      // select sequence_name from user_sequences
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val booleanJdbcType = new BooleanJdbcType
    override val byteArrayJdbcType = new ByteArrayJdbcType
    override val uuidJdbcType = new UUIDJdbcType

      /* SQLite does not have a proper BOOLEAN type. The suggested workaround is
       * INTEGER with constants 1 and 0 for TRUE and FALSE. */
      class BooleanJdbcType extends super.BooleanJdbcType {
        override def sqlTypeName = "NUMBER(1)"
        override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
      }

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
      // "select rawtohex(sys_guid()) from dual"
    }
  }
}

object OracleDriver extends OracleDriver
