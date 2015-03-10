package querki.editing

import models.{Property, PropertyBundle, Thing, Wikitext}

import querki.core.QLText
import querki.ecology._
import querki.globals._
import querki.ql.Invocation
import querki.util.QLog
import querki.values.{QLContext, SpaceState}

trait ThingEditor { self:EditorModule =>
  
  lazy val PublicUrls = interface[querki.html.PublicUrls]
  
    /**
     * How wide (in Bootstrap spans) should the editor control for this Property be?
     * 
     * If the Edit Width property is set on this Property, returns that. Otherwise, returns the
     * preferred width of the Type.
     * 
     * This is gradually going to want to get *much* more sophisticated. But it's a start.
     */
    def editorSpan(prop:Property[_,_])(implicit state:SpaceState):Int = prop.getPropOpt(editWidthProp).flatMap(_.firstOpt).getOrElse(prop.pType.editorSpan(prop)) 
    
    trait LayoutElement {
      def span:Int
      def layout:String
    }
    
    /**
     * This wrapper creates the actual layout bits for the default Instance Editor. Note that it is *highly*
     * dependent on the styles defined in main.css!
     */
    private case class EditorPropLayout(prop:Property[_,_])(implicit state:SpaceState) extends LayoutElement {
      def span = editorSpan(prop)
      def summaryTextOpt = prop.getPropOpt(Conventions.PropSummary).flatMap(_.firstOpt).map(_.text)
      def displayNameOpt:Option[String] =
        prop.getPropOpt(PromptProp) match {
          case Some(promptPV) => promptPV.firstOpt.map(_.text)
          case None => Some(prop.displayName)
        }
      def displayNamePhrase:Option[String] =
        displayNameOpt.map(displayName => summaryTextOpt match {
          case Some(summaryText) => s"""[[""$displayName"" -> _tooltip(""$summaryText"")]]"""
          case None => displayName
        })
      def layout = s"""{{span$span:
      |${displayNamePhrase.map(dnp => s"{{_propTitle: $dnp:}}").getOrElse("")}
      |
      |[[${prop.toThingId}._edit]]
      |}}
      |""".stripMargin
    }
    
    private case class EditorLinkButtonLayout() extends LayoutElement {
      def span = 12
      def layout = s"""[[_oidLink -> _mixedButton(""share-alt"", ""Done"")]]""".stripMargin
    }
    
    private case class EditorRowLayout(props:Seq[LayoutElement]) {
      def span = (0 /: props) { (sum, propLayout) => sum + propLayout.span }
      def layout = s"""{{row-fluid:
    		  |${props.map(_.layout).mkString}
              |}}
    		  |""".stripMargin
    }
    
    // This hard-coded number comes from Bootstrap, and is pretty integral to it:
    val maxSpanPerRow = 12
    
    /**
     * This takes the raw list of property layout objects, and breaks it into rows of no more
     * than 12 spans each.
     */
    private def splitRows(propLayouts:Iterable[LayoutElement]):Seq[EditorRowLayout] = {
      (Seq(EditorRowLayout(Seq.empty)) /: propLayouts) { (rows, nextProp) =>
        val currentRow = rows.last
        if ((currentRow.span + nextProp.span) > maxSpanPerRow)
          // Need a new row
          rows :+ EditorRowLayout(Seq(nextProp))
        else
          // There is room to fit it into the current row
          rows.take(rows.length - 1) :+ currentRow.copy(currentRow.props :+ nextProp)
      }
    }
    
    /**
     * This is a place to stick weird, special filters.
     */
    private def specialFilter(thing:PropertyBundle, prop:Property[_,_])(implicit state:SpaceState):Boolean = {
      // We display Default View iff it is defined locally on this Thing, or it is *not*
      // defined for the Model.
      // TBD: this is kind of a weird hack. Is it peculiar to Default View, or is there
      // a general concept here?
      if (prop == DisplayTextProp) {
        if (thing.localProp(DisplayTextProp).isDefined)
          true
        else {
          thing.getModelOpt match {
            case Some(model) => {
              val result = for (
                modelPO <- model.getPropOpt(DisplayTextProp);
                if (!modelPO.isEmpty)
                  )
                yield false
                
              result.getOrElse(true)
            }
            case None => true
          }
        }
      } else if (prop == NameProp && thing.isThing) {
        // We only should show Name if it is not derived:
        !DeriveName.nameIsDerived(thing.asThing.get, state)
      } else if (prop.id == querki.core.MOIDs.IsModelOID || prop.id == querki.types.DeriveNameMOIDs.DeriveNameOID) {
        // These are implicit properties, and we don't show them in the editor explicitly any more:
        false
      } else
        true
    }
    
    // This returns only the properties that are defined on this Thing and *not* on its Model:
    def propsNotInModel(thing:PropertyBundle, state:SpaceState):Iterable[OID] = {
      implicit val s = state
      thing.getModelOpt match {
        case Some(model) => {
          for {
            propId <- thing.props.keys
            if (!model.hasProp(propId))
          }
            yield propId
        }
        case None => Iterable.empty
      }
    }
    
    private def propsToEditForThing(thing:PropertyBundle, state:SpaceState):Iterable[Property[_,_]] = {
      implicit val s = state
      val result = for {
        propsToEdit <- thing.getPropOpt(InstanceProps)
        // If we are using the InstanceProps, then also add in the instance-local properties:
        propIds = propsToEdit.v.rawList(LinkType) ++ propsNotInModel(thing, state)
        props = propIds.map(state.prop(_)).flatten
      }
        yield props

      // Note that the toList here implicitly sorts the PropList, more or less by display name:
      result.getOrElse(PropListMgr.from(thing).toList.map(_._1).filter(specialFilter(thing, _)))
    }
    
    val thingButtons = """{{_advancedEditButton:<i class="glyphicon glyphicon-edit btn-xs _withTooltip" title="Click to open the Advanced Editor"></i>}}
      |{{_deleteInstanceButton:<i class="glyphicon glyphicon-trash btn-xs _withTooltip" title="Click to delete this"></i>}}""".stripMargin
    
    private def editorLayoutForThing(thing:PropertyBundle, state:SpaceState):QLText = {
      implicit val s = state
      thing.getPropOpt(InstanceEditViewProp).flatMap(_.v.firstTyped(LargeTextType)) match {
        // There's a predefined Instance Edit View, so use that:
        case Some(editText) => editText
        // Generate the View based on the Thing:
        case None => {
          val layoutPieces = propsToEditForThing(thing, state).map(EditorPropLayout(_))
          val layoutRows = thing match {
            // Only add the Done button if this is an actual Thing to link to:
            case t:Thing => splitRows(layoutPieces) :+ EditorRowLayout(Seq(EditorLinkButtonLayout()))
            case _ => splitRows(layoutPieces)
          }
          val propsLayout = s"""[[""{{_instanceEditor:
              |${ if (thing.isThing) thingButtons else "" }
              |${layoutRows.map(_.layout).mkString}
              |}}"" ${ if (thing.isThing) s"""-> _data(""thingId"", ""${thing.asInstanceOf[Thing].toThingId}"")""" else "" }]]
              |""".stripMargin
          QLText(propsLayout)
        }
      }
    }
    
    def instanceEditorForThing(thing:PropertyBundle, thingContext:QLContext, inv:Option[Invocation]):Wikitext = {
      implicit val state = thingContext.state
      val editText = editorLayoutForThing(thing, state)
      QL.process(editText, thingContext, inv)
    }
    
    def createInstanceButton(model:Thing, context:QLContext):Wikitext = {
      if (AccessControl.canCreate(context.state, context.request.requesterOrAnon, model.id)) {
        val label = s"Create another ${model.displayName}"
        val xml:scala.xml.Elem = <input type="button" class="_createAnother btn" data-model={model.id.toString} value={label}></input>
        HtmlUI.toWikitext(xml)
      } else {
        Wikitext("")
      }
    }
}
