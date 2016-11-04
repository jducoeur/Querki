package querki.test.functional

import models.{Collection, Property}

import querki.api.commonName

/**
 * @author jducoeur
 */
trait FuncProps { this:FuncMixin =>

  sealed trait PropExtras
  case class RestrictedToModel(model:TInstance) extends PropExtras
  
  /**
   * Represents the test's view of a Property. This is mainly focused on how to manipulate and
   * interpret it on-screen.
   */
  trait TProp[TPE <: TType] extends TThing[TProp[TPE]] {
    val tpe:TPE
    def coll:Collection
    def extras:Seq[PropExtras]
        
    def realProp(state:State) = {
      if (tid.length > 0)
        this
      else
        state.prop(this)
    }
    
    def setValue(v:tpe.TSetter)(thing:TThing[_], state:State):State = {
      // The asInstanceOf below is horrible, but I'm having trouble getting the compiler
      // to accept this as legit, and it is:
      tpe.setValue(thing, realProp(state).asInstanceOf[TProp[tpe.type]], v)
      state
    }
    
    /**
     * If relevant, do stuff just *before* creating a Property.
     */
    def prepProp(state:State):Unit = {
      tpe.prepProp(this.asInstanceOf[TProp[tpe.type]])(state)
    }
    
    /**
     * After creation, when we're in the PropertyEditor, make any needed tweaks.
     */
    def fixupProp(state:State):Unit = {
      tpe.fixupProp(this.asInstanceOf[TProp[tpe.type]])(state)
    }

  }
  
  /**
   * Represents a Property that is being created by the test, so we need to specify the
   * full details. TID gets filled in after creation.
   */
  case class TTestProp[TPE <: TType](
    display:String,
    coll:Collection,
    tpe:TPE,
    tid:TID = TID(""),
    extras:Seq[PropExtras] = Seq.empty) extends TProp[TPE]
  {
    def withTID(id:String) = copy(tid = TID(id))
  }
  
  /**
   * A test representation of a system Property. Ideally, all this stuff should be handled with
   * typeclasses, but since Properties and PTypes aren't Scala Types, I don't know how
   * to do that.
   * 
   * We'd really like to be able to infer the tpe automatically, but it needs to be done at
   * compile-time (in order to make setValue work right), and I don't see a way to do that
   * yet.
   */
  class TSystemProp[VT, RT, TPE <: TType](realProp:Property[VT,RT], val tpe:TPE, val extras:Seq[PropExtras] = Seq.empty) extends TProp[TPE] {
    def tid = realProp.id
    def display = realProp.displayName
    def coll = realProp.cType
    
    def withTID(id:String) = fail(s"Trying to set the tid of a System Property!")
  }
  
  /**
   * The actual Simple Thing object.
   */
  object SimpleThing extends TInstance("Simple Thing", querki.basic.MOIDs.SimpleThingOID, model = null)

  lazy val DefaultViewProp = new TSystemProp(IBasic.DisplayTextProp, TLargeTextType)
  lazy val NameProp = new TSystemProp(IBasic.DisplayNameProp, TTextType)
  lazy val PageHeaderProp = new TSystemProp(IUI.PageHeaderProperty, TLargeTextType)
  lazy val RestrictToModelProp = new TSystemProp(ILinks.LinkModelProp, TLinkType)
  
  /* **********************************************
   * Permissions
   */
  
  object MembersRole
    extends TInstance("Members", querki.security.MOIDs.MembersTagOID)

  lazy val WhoCanExplorePerm = new TSystemProp(IRoles.CanExplorePerm, TLinkType)
}
