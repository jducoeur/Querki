package querki.spaces

import anorm.{Success=>AnormSuccess,_}

import models.{OID, PType, SimplePTypeBuilder, UnknownOID, Wikitext}
import models.Thing.PropMap

import querki.ecology._

import querki.values.{ElemValue, QLContext, SpaceState}

object SpacePersistenceMOIDs extends EcotIds(28)

class SpacePersistenceEcot(e:Ecology) extends QuerkiEcot(e) with SpacePersistence
  with querki.core.CollectionBase
{
  // The name of the Space's Thing Table
  def thingTable(id:OID):String = "s" + sid(id)
  // The name of a backup for the Thing Table
  def backupTable(id:OID, version:Int) = thingTable(id) + "_Backup" + version
  // The name of the Space's Attachments Table
  def attachTable(id:OID) = "a" + sid(id)


  // TODO: this escape/unescape is certainly too simplistic to cope with recursive types.
  // Come back to this sometime before we make the type system more robust.
  def escape(str:String) = {
    str.replace("\\", "\\\\").replace(";", "\\;").replace(":", "\\:").replace("}", "\\}").replace("{", "\\{")
  }
  def unescape(str:String) = {
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
  def SpaceSQL(spaceId:OID, query:String, version:Int = 0):SqlQuery = {
    val replQuery = query.replace("{tname}", thingTable(spaceId)).replace("{bname}", backupTable(spaceId, version))
    SQL(replQuery)
  }
  
  def AttachSQL(spaceId:OID, query:String):SqlQuery = SQL(query.replace("{tname}", attachTable(spaceId)))
    
  def createThingInSql(thingId:OID, spaceId:OID, modelId:OID, kind:Int, props:PropMap, serialContext:SpaceState)(implicit conn:java.sql.Connection):Int = {
    SpaceSQL(spaceId, """
        INSERT INTO {tname}
        (id, model, kind, props) VALUES
        ({thingId}, {modelId}, {kind}, {props})
        """
        ).on("thingId" -> thingId.raw,
             "modelId" -> modelId.raw,
             "kind" -> kind,
             "props" -> serializeProps(props, serialContext)).executeUpdate()    
  }
  
  def serializeProps(props:PropMap, space:SpaceState):String = {
    implicit val s = space
    val serializedProps = props.map { pair =>
      val (ptr, v) = pair
      val propOpt = space.prop(ptr)
      propOpt match {
        case Some(prop) => {
          val oid = prop.id
          oid.toString + 
            ":" + 
            escape(prop.serialize(prop.castVal(v)))
        }
        case None => ""  // This is *very* weird
      }
    }
    
    serializedProps.mkString("{", ";", "}")
  }    
  
  def deserializeProps(str:String, space:SpaceState):PropMap = {
    implicit val s = space
    // Strip the surrounding {} pair:
    val stripExt = str.slice(1, str.length() - 1)
    // Note that we have to split on semicolons that are *not* preceded by backslashes. This is
    // a little tricky to express in regex -- the weird bit is saying "things that aren't backslashes,
    // non-capturing".
    val propStrs = stripExt.split("""(?<=[^\\]);""")
    val propPairs = propStrs.filter(_.trim.length() > 0).map { propStr =>
      val (idStr, valStrAndColon) = propStr.splitAt(propStr.indexOf(':'))
      val valStr = unescape(valStrAndColon.drop(1))
      val id = OID(idStr)
      val propOpt = space.prop(id)
      val v = propOpt match {
        case Some(prop) => prop.deserialize(valStr)
        case None => UnresolvedProp(UnresolvedPropType(valStr))
      }
      (id, v)
    }
    toProps(propPairs:_*)()
  }

  object UnresolvedProp extends ExactlyOneBase(UnknownOID) {
    override def makePropValue(cv:Iterable[ElemValue], pType:PType[_]):QValue = UnresPropValue(cv.toList, this, pType)
    private case class UnresPropValue(cv:implType, cType:ExactlyOneBase, pType:PType[_]) extends QValue with UnresolvedPropValue
  }
  
  // This pseudo-Type is used to store values from disk that we can't resolve yet. It is only
  // used at Space-load time:
  lazy val UnresolvedPropType = new SystemType[String](UnknownOID,
    toProps(
      setName("UnresolvedProp")
    )) with SimplePTypeBuilder[String]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = v
    def doSerialize(v:String)(implicit state:SpaceState) = v
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = Wikitext("Unresolved property value!")
  
    def doDefault(implicit state:SpaceState) = ""
  }

}