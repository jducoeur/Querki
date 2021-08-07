package querki.spaces

import scala.concurrent.Future

import anorm.{Success => AnormSuccess, _}

import models._

import querki.ecology._
import querki.time.DateTime
import querki.time.TimeAnorm._
import querki.util.QLog
import querki.values.{ElemValue, EmptyValue, QLContext, SpaceState}

object SpacePersistenceMOIDs extends EcotIds(28)

class SpacePersistenceEcot(e: Ecology) extends QuerkiEcot(e) with SpacePersistence with querki.core.CollectionBase {
  // The name of the Space's Thing Table
  def thingTable(id: OID): String = "s" + sid(id)

  // The name of a backup for the Thing Table
  def backupTable(
    id: OID,
    version: Int
  ) = thingTable(id) + "_Backup" + version
  // The name of the Space's Conversations Table
  def convTable(id: OID) = "c" + sid(id)
  // The name of the Space's User Values Table
  def userValuesTable(id: OID) = "uv" + sid(id)
  // The name for a pre-extraction backup
  def preExtractBackup(id: OID) = thingTable(id) + "_preExtract" + DateTime.now.getMillis

  // TODO: this escape/unescape is certainly too simplistic to cope with recursive types.
  // Come back to this sometime before we make the type system more robust.
  def escape(str: String) = {
    str.replace("\\", "\\\\").replace(";", "\\;").replace(":", "\\:").replace("}", "\\}").replace("{", "\\{")
  }

  def unescape(str: String) = {
    str.replace("\\{", "{").replace("\\}", "}").replace("\\:", ":").replace("\\;", ";").replace("\\\\", "\\")
  }

  /**
   * The intent here is to use this with queries that use the thingTable. You can't use
   * on()-style parameters for table names (because on() quotes the params in a way that makes
   * MySQL choke), so we need to work around that.
   *
   * You can always use this in place of ordinary SQL(); it is simply a no-op for ordinary queries.
   *
   * If you need to use the {bname} parameter, you must pass in a version number.
   */
  def SpaceSQL(
    spaceId: OID,
    query: String,
    version: Int = 0
  ): SqlQuery = {
    val replQuery =
      query.replace("{tname}", thingTable(spaceId)).replace("{bname}", backupTable(spaceId, version)).replace(
        "{cname}",
        convTable(spaceId)
      ).replace("{uvname}", userValuesTable(spaceId)).replace("{exname}", preExtractBackup(spaceId))
    SQL(replQuery)
  }

  def createThingInSql(
    thingId: OID,
    spaceId: OID,
    modelId: OID,
    kind: Int,
    props: PropMap,
    modTime: DateTime,
    serialContext: SpaceState
  )(implicit
    conn: java.sql.Connection
  ): Int = {
    val sql = SpaceSQL(
      spaceId,
      """
        INSERT INTO {tname}
        (id, model, kind, modified, props) VALUES
        ({thingId}, {modelId}, {kind}, {modTime}, {props})
        """
    ).on(
      "thingId" -> thingId.raw,
      "modelId" -> modelId.raw,
      "kind" -> kind,
      "modTime" -> modTime,
      "props" -> serializeProps(props, serialContext)
    )
    sql.executeUpdate()
  }

  def serializeProps(
    props: PropMap,
    space: SpaceState
  ): String = {
    implicit val s = space
    val serializedProps = props.map { pair =>
      val (ptr, v) = pair
      val propOpt = space.prop(ptr)
      propOpt match {
        case Some(prop) => {
          val oid = prop.id
          oid.toString +
            ":" +
            escape(prop.serialize(v))
        }
        case None => {
          // This is *very* weird, and typically means that an Ecot has failed to register a Property in System Space:
          QLog.error(s"Attempting to serialize Property $ptr, but could not find it in Space ${space.displayName}")
          s"$ptr:"
        }
      }
    }

    serializedProps.mkString("{", ";", "}")
  }

  def deserializeProp(propStr: String)(implicit space: SpaceState): (OID, QValue) = {
    try {
      val (idStr, valStrAndColon) = propStr.splitAt(propStr.indexOf(':'))
      val valStr = unescape(valStrAndColon.drop(1))
      val id = OID(idStr)
      val propOpt = space.prop(id)
      val v = propOpt match {
        case Some(prop) => prop.deserialize(valStr)
        case None       => UnresolvedProp(UnresolvedPropType(valStr))
      }
      (id, v)
    } catch {
      case e: Exception => {
        QLog.error(s"""Exception while trying to deserialize property string "$propStr":""", e)
        (UnknownOID, EmptyValue.untyped)
      }
    }
  }

  def deserializeProps(
    str: String,
    space: SpaceState
  ): PropMap = {
    implicit val s = space
    // Strip the surrounding {} pair:
    val stripExt = str.slice(1, str.length() - 1)
    // Note that we have to split on semicolons that are *not* preceded by backslashes. This is
    // a little tricky to express in regex -- the weird bit is saying "things that aren't backslashes,
    // non-capturing".
    val propStrs = stripExt.split("""(?<=[^\\]);""")
    val propPairs = propStrs.filter(_.trim.length() > 0).map(deserializeProp(_))
    toProps(propPairs: _*)
  }

  def recordUnresolvedProp(valStr: String): QValue = UnresolvedProp(UnresolvedPropType(valStr))

  object UnresolvedProp extends ExactlyOneBase(UnknownOID) {

    override def makePropValue(
      cv: Iterable[ElemValue],
      pType: PType[_]
    ): QValue = UnresPropValue(cv.toList, this, pType)

    private case class UnresPropValue(
      cv: implType,
      cType: ExactlyOneBase,
      pType: PType[_]
    ) extends QValue
         with UnresolvedPropValue
  }

  // This pseudo-Type is used to store values from disk that we can't resolve yet. It is only
  // used at Space-load time:
  lazy val UnresolvedPropType = new SystemType[String](
    UnknownOID,
    toProps(
      setName("UnresolvedProp")
    )
  ) with SimplePTypeBuilder[String] {
    def doDeserialize(v: String)(implicit state: SpaceState) = v
    def doSerialize(v: String)(implicit state: SpaceState) = v

    def doWikify(
      context: QLContext
    )(
      v: String,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) =
      Future.successful(Wikitext("Unresolved property value!"))

    def doDefault(implicit state: SpaceState) = ""
    // Transient, so we don't care:
    def doComputeMemSize(v: String): Int = 0
  }

}
