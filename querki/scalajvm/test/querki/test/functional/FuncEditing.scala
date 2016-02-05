package querki.test.functional

/**
 * Editor-specific functions. These are broken into their own layer because there are so many of them.
 * 
 * @author jducoeur
 */
trait FuncEditing { this:FuncMixin =>
  
  def chooseModel(thing:TInstance) = {
    waitFor("_modelSelected")
    singleSel("_modelSelector").value = thing.model.tid.underlying
    click on "_modelSelected"    
  }
  
  def editorId(thing:TThing[_], prop:TProp[_]):String = s"v-${prop.oidStr}-${thing.oidStr}"
  
  /**
   * Design a new Model.
   * 
   * @param ops Any number of operations to perform inside the Advanced Editor. Setting the Name is
   *   automatic, but everything else needs to be specified. 
   */
  def designAModel(thing:TInstance, ops:State => State*)(state:State):State = {
    spew(s"Designing Model ${thing.display}")
    // Choose "Design a Model" from the menu, and select the correct Model from the popup dialog:
    clickMenuItem(DesignModelItem)
    chooseModel(thing)
    
    // Fill out the Model Designer, based on the specified properties:
    val page = ModelDesigner(thing)
    // Note that this page sometimes uses thingTitle instead, but shouldn't when we get to it from here:
    val expectedTitle = page.msg("pageTitle", ("modelName" -> ""))
    // Note that we have to use include instead of be, because we haven't set the name yet, so it's
    // going to use the TID instead:
    eventually { pageTitle should include (expectedTitle) }
    // Note the explicit assumption that the TID is at the end of the title here. That's suspicious, and
    // may not survive in the long run.
    val instanceTID = pageTitle.drop(expectedTitle.length)
    val thingWithTID = thing.withTID(instanceTID).copy(isModel = true)
    val stateWithThing = state.updateSpace(space => space.copy(things = space.things :+ thingWithTID))
    NameProp.setValue(thingWithTID, thing.display)
    val stateAfterOps = run(stateWithThing, ops:_*)
    click on "_doneDesigning"
    waitUntilCreated(thing)
    stateAfterOps
  }
  
  def getInstanceSection():Element = eventually { 
    val sec = find(xpath("//ul[contains(@class, '_propertySection')]"))
    sec should not be empty
    sec.get
  }
  
  /**
   * Create a Property inside the Model Designer.
   * 
   * This expects to be called as one of the ops inside designAModel().
   */
  def createProperty[TPE <: TType](prop:TProp[TPE])(state:State):State = {
    spew(s"Creating Property ${prop.display}")
    
    val instanceSection = getInstanceSection()
    
    def countEditors():Int = {
      val editors = instanceSection.findAll(xpath("./li[contains(@class, '_instanceEditor')]"))
      editors.size
    }
    
    // First, we need to look at the PropValueEditors already showing, since we'll
    // need to wait until another one is added:
    val nOrigEditors = countEditors()
    
    // What is currently showing?
    if (find("_doCreatePropertyButton").isDefined) {
      // Don't need to do anything -- the dialog's already open
    } else if (find("_createInstead").isDefined) {
      // The Add Existing Property dialog is open; switch boxes
      click on "_createInstead"
      waitFor("_doCreatePropertyButton")
    } else if (find("_addPropertyButton").isDefined) {
      click on "_addPropertyButton"
      waitFor("_createInstead")
      click on "_createInstead"
      waitFor("_doCreatePropertyButton")
    } else
      fail(s"Couldn't figure out where we are when trying to create Property ${prop.display}")
      
    // Set the name...
    textField("_createPropName").value = prop.display
    // ... the collection...
    click on s"_coll${prop.coll.tid.underlying}"
    // ... the type...
    singleSel("_typeSelector").value = prop.tpe.tid.underlying
    // ... and actually create it:
    click on "_doCreatePropertyButton"
    
    // Wait for the new PropValueEditor to be created:
    eventually { countEditors() should equal (nOrigEditors + 1) }
    // The new Property's TID can be found in it:
    val propEditor = instanceSection.find(xpath("./li[last()]")).getOrElse(fail("Couldn't find PropValueEditor for newly-created Property!"))
    val propTID = propEditor.getAttribute("data-propid")
    val propWithTID = prop.withTID(propTID)
    spew(s"Created Property ${prop.display} as $propTID")
    val stateWithProp = state.updateSpace(space => space.copy(props = space.props :+ propWithTID))
    propWithTID.fixupProp(stateWithProp)
    stateWithProp
  }
  
  /**
   * Creates an Instance, using the Create any Thing menu pick.
   */
  def createAnyThing(thing:TInstance)(state:State):State = {
    spew(s"Creating Instance ${thing.display}")
    // Choose "Create any Thing" from the menu, and select the correct Model from the popup dialog:
    clickMenuItem(CreateThingItem)
    chooseModel(thing)
    
    // Fill out the CreateAndEdit page:
    waitForTitle(CreateAndEdit(thing.model))
    val editor = find(className("_instanceEditor")).getOrElse(fail("Couldn't find instance editor for newly-created Thing!"))
    val instanceTID = editor.attribute("data-thingid").getOrElse(fail("Couldn't find TID for newly-created Thing!"))
    val thingWithTID = thing.withTID(instanceTID)
    // Fill in the name property:
    NameProp.setValue(thingWithTID, thing.display)
    
    // Click "Done", and update the State with the newly-created Thing:
    click on "_editDoneButton"
    val t = waitUntilCreated(thingWithTID)
    state.updateSpace(space => space.copy(things = space.things :+ t))
  }
}
