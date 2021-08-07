package querki.collections

import models.ThingState

import querki.ecology._
import querki.globals._
import querki.types.{ModelTypeDefiner, SimplePropertyBundle}
import querki.values.EmptyValue

object GroupingMOIDs extends EcotIds(36) {
  val GroupModelOID = moid(1)
  val GroupKeyPropOID = moid(2)
  val GroupMembersPropOID = moid(3)
  val GroupTypeOID = moid(4)
  val GroupByFunctionOID = moid(5)
  val GroupGetOID = moid(6)
  val GroupElementsOID = moid(7)
}
import GroupingMOIDs._

/**
 * This rather specialized Ecot defines the _groupBy Function, and all the attendant bits. It was
 * originally in the CollectionsModule, but turned out to be big enough to be worth an Ecot to itself.
 * Some of its ideas might yet wind up getting refactored or exposed elsewhere, but for now it is
 * nicely self-contained.
 */
class GroupingEcot(e: Ecology)
  extends QuerkiEcot(e)
     with querki.core.MethodDefs
     with ModelTypeDefiner
     with EcologyMember {

  val Types = initRequires[querki.types.Types]

  /**
   * *********************************************
   * PROPERTIES
   * *********************************************
   */

  lazy val groupKeyProperty = new SystemProperty(
    GroupKeyPropOID,
    Types.WrappedValueType,
    Optional,
    toProps(
      setName("_groupKey"),
      setInternal,
      Categories(CollTag),
      Summary("The key of a single grouping that comes from _groupBy")
    )
  )

  /**
   * This is the heart of what is limiting us to just Things on the input. If this became WrappedValueType
   * instead, we could work with anything. But that requires beefing up WrappedValueType to have sensible
   * delegation of all the standard PType methods in order for the contents to be useful.
   */
  lazy val groupMembersProperty = new SystemProperty(
    GroupMembersPropOID,
    Types.WrappedValueType,
    QList,
    toProps(
      setName("_groupMembers"),
      setInternal,
      Categories(CollTag),
      Summary("The elements in a single grouping from _groupBy; use _groupElements to access these.")
    )
  )

  /**
   * *********************************************
   * TYPES
   * *********************************************
   */

  lazy val groupModel = ThingState(
    GroupModelOID,
    systemOID,
    RootOID,
    toProps(
      setName("_groupModel"),
      setInternal,
      Categories(CollTag),
      Summary("The Model of the values you get from _groupBy"),
      groupKeyProperty(Core.QNone),
      groupMembersProperty()
    )
  )

  override lazy val things = Seq(
    groupModel
  )

  lazy val groupType = new ModelType(
    GroupTypeOID,
    GroupModelOID,
    toProps(
      setName("_groupType"),
      setInternal,
      Categories(CollTag),
      Summary("The Type of the values you get from _groupBy")
    )
  )

  override lazy val types = Seq(
    groupType
  )

  /**
   * *********************************************
   * FUNCTIONS
   * *********************************************
   */

  lazy val groupElementsFunction = new InternalMethod(
    GroupElementsOID,
    toProps(
      setName("_groupElements"),
      SkillLevel(SkillLevelAdvanced),
      Categories(CollTag),
      Summary("Fetches the values in a _groupBy group."),
      Signature(
        expected = Some(Seq(groupType), "A group from _groupBy"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (AnyType, "The values in that group.")
      )
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      implicit val s = inv.state

      // All this really does is unwrap the _groupMembers, so that you can use the real types:
      for {
        group <- inv.contextAllAs(groupType)
        member <- inv.iter(group.getPropAll(groupMembersProperty))
      } yield member
    }
  }

  lazy val groupByFunction = new InternalMethod(
    GroupByFunctionOID,
    toProps(
      setName("_groupBy"),
      SkillLevel(SkillLevelAdvanced),
      Categories(CollTag),
      Summary("Groups the received Things by the specified Property or Expression"),
      Signature(
        expected = Some(Seq(AnyType), "A List of values to be grouped"),
        reqs =
          Seq(("groupExp", AnyType, "The expression to apply to each received element, saying what to group it on")),
        opts = Seq.empty,
        returns = (AnyType, "A List of _groupModel values.")
      ),
      Details("""This Function takes a list of Things, and collects them into groups based on the given Expression.
                |The Expression may in principle be anything, but is most often a Property on the Things.
                |
                |The result of this is a List of _groupModel values. _groupModel has two Properties:
                |* _groupKey -- the key that identifies everything in this group
                |* _groupMembers -- the list of values in this group
                |
                |Don't use _groupMembers directly, though (it's hard to work with); instead, use the
                |`_groupElements` function, which will fetch the elements the way you expect.
                |
                |For example, say that My Model has a Property named Score, which is a number from 1-5.
                |I can separate out all of the Instances of My Model based on Score, and print each group,
                |by saying:
                |```
                |\[[My Model._instances -> _groupBy(Score) -> 
                |  \""**Score:** \[[_groupKey\]]   **Members:** \[[_groupElements -> _sort -> _commas\]]\""\]]
                |```
                |This Function is still pretty delicate. If the parameter doesn't evaluate properly on
                |all of the values, you will likely get an error.
                |
                |ADVANCED: in principle, the data structure returned by _groupBy is a Map. It is somewhat likely
                |that, somewhere down the line, we will add Map as an official Collection, and rewrite this in
                |terms of that.""".stripMargin)
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      val keyThingsWrapped = for {
        elemContext <- inv.contextElements
        key <- inv.process("groupExp", elemContext)
      } yield (key, elemContext.value)

      def sortFunc(
        left: (QValue, QValue),
        right: (QValue, QValue)
      ): Boolean = {
        if (left._1.isEmpty && !right._1.isEmpty)
          // *Strictly* less-than, so it's only true iff only the left is empty:
          true
        else if (right._1.isEmpty)
          false
        else {
          val pt = left._1.pType
          val sortResult =
            for {
              leftVal <- left._1.firstOpt
              rightVal <- right._1.firstOpt
            } yield pt.comp(inv.context)(leftVal, rightVal)

          sortResult.getOrElse(false)
        }
      }

      def keysMatch(
        curKey: QValue,
        nextKey: QValue
      ): Boolean = {
        if (curKey.isEmpty)
          nextKey.isEmpty
        else if (nextKey.isEmpty)
          curKey.isEmpty
        else
          curKey.pType.matches(curKey.first, nextKey.first)
      }

      for {
        keyThingPairs <- keyThingsWrapped.get.map(_.toSeq)
        // We need to cope with missing keys with going kaboom. So if we find a missing key, we
        // normalize it to the default value for the type:
        realTypeOpt = keyThingPairs.find(!_._1.isEmpty).map(_._1.pType)
        fixedPairs = realTypeOpt match {
          case Some(pt) => keyThingPairs.map { pair =>
              val (key, id) = pair
              if (key.isEmpty)
                (EmptyValue(pt), id)
              else
                (key, id)
            }
          case None => keyThingPairs
        }
        sortedPairs = fixedPairs.sortWith(sortFunc)
        rawGroupings = (Seq.empty[(QValue, Seq[QValue])] /: sortedPairs) { (seqs, pair) =>
          val (key, id) = pair
          if (seqs.isEmpty)
            Seq((key, Seq(id)))
          else {
            val current = seqs.last
            val (curKey, curIds) = current
            if (keysMatch(curKey, key))
              seqs.dropRight(1) :+ (curKey, curIds :+ id)
            else
              seqs :+ (key, Seq(id))
          }
        }
        groupings = rawGroupings.map { raw =>
          val (key, ids) = raw
          groupType(SimplePropertyBundle(
            groupKeyProperty(key),
            groupMembersProperty(ids: _*)
          ))
        }
      } yield QList.makePropValue(groupings, groupType)
    }
  }

  lazy val groupGet = new InternalMethod(
    GroupGetOID,
    toProps(
      setName("_groupGet"),
      SkillLevel(SkillLevelAdvanced),
      Categories(CollTag),
      Summary("Produces the Things from the specified group."),
      Signature(
        expected = Some(Seq(groupType), "The results from _groupBy"),
        reqs = Seq(("key", AnyType, "The key of the group you want to fetch")),
        opts = Seq.empty,
        returns = (AnyType, "The Things that match that key.")
      )
    )
  ) {

    override def qlApply(inv: Invocation): QFut = {
      implicit val s = inv.state
      for {
        targetKey <- inv.process("key")
        group <- inv.contextAllAs(groupType)
        groupKeyPV <- inv.opt(group.getPropOpt(groupKeyProperty))
        // Note that we can't use the usual PV.firstOpt here. That's for type-matching reasons: the
        // Property is WrappedValueType, but the actual value is of the real, underlying type.
        groupKey = groupKeyPV.v
        if (groupKey.matches(targetKey))
        memberPV <- inv.opt(group.getPropOpt(groupMembersProperty))
        member <- inv.iter(memberPV.rawList)
      } yield member
    }
  }

  override lazy val props = Seq(
    groupKeyProperty,
    groupMembersProperty,
    groupElementsFunction,
    groupByFunction,
    groupGet
  )

}
