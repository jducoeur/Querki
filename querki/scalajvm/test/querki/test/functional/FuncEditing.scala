package querki.test.functional

/**
 * Editor-specific functions. These are broken into their own layer because there are so many of them.
 * 
 * @author jducoeur
 */
trait FuncEditing { this:FuncMixin =>
  
  def chooseModel(thing:TInstance)(state:State) = {
    waitFor("_modelSelected")
    val modelTid = state.tid(thing.model)
    singleSel("_modelSelector").value = modelTid.underlying
    click on "_modelSelected"    
  }
  
  def editorId(thing:TThing[_], prop:TProp[_]):String = s"v-${prop.oidStr}-${thing.oidStr}"
  
  def runSetters(t:TThing[_], initialState:State, ops:(TThing[_], State) => State*):State = {
    (initialState /: ops) { (state, op) =>
      op(t, state)
    }
  }
  
  /**
   * Edit an existing Model. Assumes that we are currently looking at that Model's Thing Page.
   * 
   * This is similar to designAModel(), but exists solely for the provided ops.
   */
  def editModel(setters:(TThing[_], State) => State*)(state:State):State = {
    state.currentPage match {
      case ThingPage(thing) if (thing.isInstanceOf[TInstance]) => {
        // I *should* be able to do this as part of the above case, but I'm having trouble finding
        // the right syntax:
        val model = thing.asInstanceOf[TInstance]
        spew(s"Editing Model $model")
        if (!model.isModel)
          fail(s"Called editModel on non-Model $model")
          
        // Open the Model Editor:
        click on "_thingEdit"
        val page = ModelDesigner(model)
        waitForTitle(page)
        val stateAfterOps = runSetters(model, state -> page, setters:_*)
        // Take the setters into account before waitForThing():
        val newModel = stateAfterOps.thing(model)
        click on "_doneDesigning"
        waitForThing(newModel)(stateAfterOps)
      }
      case _ => fail(s"editModel should be called from the Model's page, was called instead on ${state.currentPage}")
    }
  }
  
  /**
   * Design a new Model.
   * 
   * @param ops Any number of operations to perform inside the Advanced Editor. Setting the Name is
   *   automatic, but everything else needs to be specified. 
   */
  def designAModel(thing:TInstance, setters:(TThing[_], State) => State*)(state:State):State = {
    spew(s"Designing Model ${thing.display}")
    // Choose "Design a Model" from the menu, and select the correct Model from the popup dialog:
    clickMenuItem(DesignModelItem)
    chooseModel(thing)(state)
    
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
    val stateWithThing = state.updateSpace(space => space.copy(things = space.things + (thingWithTID.tid -> thingWithTID)))
    NameProp.setValue(thing.display)(thingWithTID, stateWithThing)
    val stateAfterOps = runSetters(thingWithTID, stateWithThing, setters:_*)
    click on "_doneDesigning"
    waitUntilCreated(thing)
    stateAfterOps -> ThingPage(thingWithTID)
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
  def createProperty[TPE <: TType](prop:TProp[TPE])(thing:TThing[_], state:State):State = {
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
    val stateWithProp = state.updateSpace(space => space.copy(props = space.props + (propWithTID.tid -> propWithTID)))
    propWithTID.fixupProp(stateWithProp)
    stateWithProp
  }
  
  /**
   * Creates an Instance, using the Create any Thing menu pick.
   */
  def createAnyThing(thing:TInstance, setters:(TThing[_], State) => State*)(state:State):State = {
    spew(s"Creating Instance ${thing.display}")
    // Choose "Create any Thing" from the menu, and select the correct Model from the popup dialog:
    clickMenuItem(CreateThingItem)
    chooseModel(thing)(state)
    
    // Fill out the CreateAndEdit page:
    waitForTitle(CreateAndEdit(thing.model))
    val editor = find(className("_instanceEditor")).getOrElse(fail("Couldn't find instance editor for newly-created Thing!"))
    val instanceTID = editor.attribute("data-thingid").getOrElse(fail("Couldn't find TID for newly-created Thing!"))
    val thingWithTID = thing.withTID(instanceTID)
    val stateWithThing = state.updateSpace(space => space.copy(things = space.things + (thingWithTID.tid -> thingWithTID)))
    // Fill in the name property:
    NameProp.setValue(thing.display)(thingWithTID, stateWithThing)
    
    // Do any additional operations (usually setting other Properties):
    setters.map(f => f(thingWithTID, stateWithThing))
    
    // Click "Done", and update the State with the newly-created Thing:
    click on "_editDoneButton"
    waitUntilCreated(thingWithTID)
    stateWithThing -> ThingPage(thingWithTID)
  }
}
