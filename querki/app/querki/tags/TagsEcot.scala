package querki.tags

import querki.ecology._

import models.system.{InternalMethod, SingleContextMethod}
import models.system.{LinkFromThingBuilder, NameType, NameableType, NewTagSetType, PlainText, TagSetType}

import ql._
import querki.values._

object MOIDs extends EcotIds(22) {
  val TagRefsOID = sysId(72)
  val TagsForPropertyOID = sysId(74)
}

/**
 * TODO: this should probably absorb more of the concept of "tags", maybe even including the Types.
 */
class TagsEcot(e:Ecology) extends QuerkiEcot(e) with Tags {
  import MOIDs._
  
  def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String] = {
    implicit val s = space
    val thingsWithProp = space.thingsWithProp(propIn)
    if (propIn.pType == TagSetType) {
      val prop = propIn.confirmType(TagSetType)
      (Set.empty[String] /: thingsWithProp) { (set, thing) =>
        set ++ thing.getProp(prop).rawList
      }
    } else if (propIn.pType == NewTagSetType) {
      val prop = propIn.confirmType(NewTagSetType)
      (Set.empty[String] /: thingsWithProp) { (set, thing) =>
        set ++ thing.getProp(prop).rawList.map(_.text)
      }      
    } else
      // TODO: should this be a PublicException?
      throw new Exception("Trying to fetchTags on a non-Tag Property!")
  }

  /***********************************************
   * FUNCTIONS
   ***********************************************/  

  class TagRefsMethod extends InternalMethod(TagRefsOID,
    toProps(
      setName("_tagRefs"),
      Summary("Produces a List of all Things that have the received Thing or Name as a Tag"),
      Details("""    NAME -> _tagRefs -> THINGS
          |_tagRefs is usually the right way to answer the question "what points to this?" For example, if I wanted to
          |show a bullet list of everything that points to the current Thing, I would simply say:
          |    _tagRefs -> _bulleted
          |_tagRefs is designed to receive a "name" (which is what a Tag is). If you send it a Thing, then it will use
          |the Name of that Thing.
          |
          |NOTE: _tagRefs and _refs are closely related concepts. They currently work differently, but we might
          |at some point combine them for simplicity.""".stripMargin)))
  { 
    override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
      val elemT = context.value.pType
      elemT match {
        case nameable:NameableType => {
          val allProps = context.state.allProps.values
          val tagProps = allProps.filter(prop => prop.pType == TagSetType || prop.pType == NewTagSetType)
          val name = nameable.getName(context)(context.value.first)
          val thingOpt = elemT match {
            case LinkType => LinkType.followLink(context)
            case _ => None
          }
          val namePt = thingOpt.map(thing => PlainText(thing.unsafeDisplayName)).getOrElse(PlainText(name))
          val candidates = context.state.allThings
        
          def hasThisTag(candidate:Thing):Boolean = {
            tagProps.exists{ prop =>
              val propAndVal = candidate.localProp(prop)
              val found = prop.pType match {
                case TagSetType => {
	              val candidateTags:Option[List[String]] = propAndVal.map(_.v.rawList(TagSetType))
	              candidateTags.map(_.exists { candidateName => NameType.equalNames(candidateName, name) })
                }
                case NewTagSetType => {
	              val candidateTags:Option[List[PlainText]] = propAndVal.map(_.v.rawList(NewTagSetType))
	              candidateTags.map(_.exists { candidateName => NewTagSetType.equalNames(candidateName, namePt) })                
                }
              }
              found.getOrElse(false)
            }
          }
        
          QList.from(candidates.filter(hasThisTag), LinkFromThingBuilder)
        }
        case _ => WarningValue("_tagRefs can only be used with a Tag or Link, not " + elemT.displayName)
      }
    }
  }

  class TagsForPropertyMethod extends SingleContextMethod(TagsForPropertyOID,
    toProps(
      setName("_tagsForProperty"),
      Summary("Show all the Tags that are defined for this Property"),
      // TODO: this isn't displaying properly. Why not? It looks like the "" nested inside of the indirect
      // Property is causing the problem -- I am getting a syntax error *claiming* to be in Default View,
      // pointing at the first "":
      Details("""    TAG PROPERTY._tagsForProperty -> LIST OF TAGS
          |_tagsForProperty can be used on any Property whose Type is Tag Set. It produces a list of all of the
          |tags that have been used in that Property so far.
          |
          |Typically, you then feed the results of this to _tagRefs, to get the Things that use that Tag. For example,
          |if I had a list of Wines, using a Tag Set Property giving its "Wine Color", I could say:
          |    \[[Wine Color._tagsForProperty -> \""* \____: \[[_tagRefs -> _commas\]]\""\]]
          |to produce a list like:
          |* Red: Pinot Noir, Shiraz
          |* White: Pinot Gris, Chardonnay
          |""".stripMargin)))
  {
    def fullyApply(mainContext:QLContext, partialContext:QLContext, params:Option[Seq[QLPhrase]]):QValue = {
      applyToIncomingThing(partialContext) { (shouldBeProp, _) =>
        shouldBeProp match {
          case prop:Property[_,_] if (prop.pType == TagSetType) => {
            QList.from(fetchTags(partialContext.state, prop), TagSetType)
          }
          case _ => WarningValue("The _tagsForProperty method can only be used on Tag Set Properties")
        } 
      }    
    }
  }

  override lazy val props = Seq(
    new TagRefsMethod,
    new TagsForPropertyMethod
  )
}