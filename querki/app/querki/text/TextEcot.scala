package querki.text

import querki.ecology._

import models.Wikitext

import querki.ql.QLPhrase
import querki.values.{QLContext}

object MOIDs extends EcotIds(23) {
  val PluralizeOID = sysId(54)
  val JoinMethodOID = sysId(65)
}

class TextEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import MOIDs._
  
  lazy val QL = interface[querki.ql.QL]

  /***********************************************
   * FUNCTIONS
   ***********************************************/
	
	lazy val PluralizeMethod = new InternalMethod(PluralizeOID,
	    toProps(
	      setName("_pluralize"),
	      Summary("Produces the right word depending on how many elements are in a collection."),
	      Details("""    RECEIVED -> _pluralize(SINGULAR,PLURAL)
	          |This is a convenient method for choosing different text depending on a Property. The RECEIVED
	          |Context should usually be a List. If it contains a single element, _pluralize produces
	          |SINGULAR; if it contains multiple *or* zero elements, _pluralize produces PLURAL.
	    	  |
	          |Note that this behaviour is pretty English-specific. We expect that other variations will
	          |be needed for other languages in the long run.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    def chooseParam(params:Seq[QLPhrase]):QLPhrase = {
	      val received = context.value
	      if (received.isEmpty || received.size > 1)
	        params(1)
	      else
	        params(0)
	    }
	    
	    val result = for
	    (
	      params <- paramsOpt if params.length == 2;
	      phrase = chooseParam(params);
	      parser <- context.parser
	    )
	      yield parser.processPhrase(phrase.ops, context.asCollection).value
	      
	    result.getOrElse(WarningValue("_pluralize requires exactly two parameters"))
	  }
	}
	
	lazy val JoinMethod = new InternalMethod(JoinMethodOID,
	    toProps(
	      setName("_join"),
	      Summary("Combine a list of Text values together"),
	      Details("""    LIST -> _join(OPEN, SEP, CLOSE) -> QTEXT
	          |_join takes the given LIST, and turns it into a single line. For example, if My List was "Cat", "Dog", "Horse",
	          |then
	          |    My List -> _join
	          |would come out as "CatDogHorse".
	          |
	          |Of course, that probably isn't what you want -- most of the time, you want some separators at the beginning,
	          |middle and end. Those are the parameters; how many parameters you give define how they are used. If there is
	          |only one, then it is SEP, the separator in between elements. So
	          |    My List -> _join(", ")
	          |would come out as "Cat, Dog, Horse" -- more reasonable.
	          |
	          |If there are two parameters, then they are OPEN and SEP. So for example, if I wanted to include dashes at the
	          |beginning, that would be:
	          |    My List -> _join("-- ", ", ")
	          |which would come out as "-- Cat, Dog, Horse". And if I wanted parentheses around the entire list, I'd use all
	          |three parameters -- OPEN, SEP and CLOSE -- as:
	          |    My List -> _join("(", ", ", ")")
	          |to get "(Cat, Dog, Horse)".
	          |
	          |Note that you can use _join with anything, not just Text -- if the received values aren't Text, then they will
	          |be rendered into their default forms before getting combined. But at the end of _join, what you get back is
	          |one big block of QText. You can't do any further processing on the elements after this.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    val (openPhrase, sepPhrase, closePhrase) = paramsOpt match {
	      case Some(params) if (params.length == 1) => (None, Some(params(0)), None)
	      case Some(params) if (params.length == 2) => (Some(params(0)), Some(params(1)), None)
	      case Some(params) if (params.length > 2) => (Some(params(0)), Some(params(1)), Some(params(2)))
	      case _ => (None, None, None)
	    }
	    def renderParam(paramOpt:Option[QLPhrase]):Wikitext = {
	      paramOpt match {
	        case Some(param) => {
	          val collContext = context.asCollection
	          val paramVal = context.parser.get.processPhrase(param.ops, collContext).value
	          val renderedParam = paramVal.pType.wikify(context)(paramVal.first)
	          renderedParam
	        }
	        case _ => Wikitext.empty
	      }
	    }
	
	    val elemT = context.value.pType
	    val renderedList = context.value.cv.map{elem => elemT.wikify(context)(elem)}
	    val result =
	      if (renderedList.isEmpty) {
	        Wikitext.empty
	      } else {
	        val sep = renderParam(sepPhrase)
	        renderParam(openPhrase) + (renderedList.head /: renderedList.tail) ((total, next) => total + sep + next) + renderParam(closePhrase)
	      }
	    QL.WikitextValue(result)
	  }
	}

  override lazy val props = Seq(
    PluralizeMethod,
    JoinMethod
  )
}