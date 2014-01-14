package querki

import models.{PType, PTypeBuilder, Wikitext}

import querki.core.QLText
import querki.ecology._

import querki.values.{ElemValue, QLContext, QValue}

package object ql {

  /**
   * This trait should be used by any type that can consider itself "code" -- in particular, that wants to be
   * displayable in the _code() method. 
   */
  trait CodeType {
    def code(elem:ElemValue):String
  }

  /**
   * A QLFunction is something that can be called in the QL pipeline. It is mostly implemented
   * by Thing (with variants for Property and suchlike), but can also be anonymous -- for example,
   * when a method returns a partially-applied function.
   */
  trait QLFunction {
    def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue
  }
  
  trait QL extends EcologyInterface {
    
    /**
     * The primary entry point for processing a body of QLText into Wikitext.
     * 
     * The input text should be a block of QLText (with the text on the "outside"). This parses
     * that, uses the given context and params to process it, and returns the resulting Wikitext.
     */
    def process(input:QLText, ci:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):Wikitext
    
    /**
     * Process a QL Function into a QValue.
     * 
     * The input text should be a block of QL (with any text on the "inside"). This parses that,
     * uses the given context and params to process it, and returns the resulting QValue.
     */
    def processMethod(input:QLText, ci:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue
    
    def UnknownNameType:PType[String] with PTypeBuilder[String,String]
    def ParsedTextType:PType[Wikitext] with PTypeBuilder[Wikitext,Wikitext]
    
    def WarningValue(msg:String):QValue
    def ErrorValue(msg:String):QValue
    def WikitextValue(wikitext:Wikitext):QValue
    
    def EmptyListCut():QValue
  }

}