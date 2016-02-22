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
    singleSel("_modelSelector").value = modelTid
    click on "_modelSelected"    
  }
  
  def editorId(thing:TThing[_], prop:TProp[_]):String = s"v-${prop.oidStr}-${thing.oidStr}"
  
  def runSetters(t:TThing[_], initialState:State, ops:(TThing[_], State) => State*):State = {
    (initialState /: ops) { (state, op) =>
      op(t, state)
    }
  }
  
  /**
   * Edit an existing Model or Thing. Assumes that we are currently looking at that Thing's Page.
   * 
   * This is similar to designAModel(), but exists solely for the provided ops.
   */
  def editModelGuts(opener: => Unit, setters:(TThing[_], State) => State*)(state:State):State = {
    def doEdit(thing:TInstance):State = {
      spew(s"Editing thing")
        
      // Open the Model Editor:
      opener
      val page = ModelDesigner(thing)
      waitForTitle(page)
      val stateAfterOps = runSetters(thing, state -> page, setters:_*)
      // Take the setters into account before waitForThing():
      val newModel = stateAfterOps.thing(thing)
      click on "_doneDesigning"
      waitForThing(newModel)(stateAfterOps)      
    }
    
    state.currentPage match {
      case ThingPage(inst) if (inst.isInstanceOf[TInstance]) => {
        // I *should* be able to do this as part of the above case, but I'm having trouble finding
        // the right syntax:
        val thing = inst.asInstanceOf[TInstance]
        doEdit(thing)
      }
      
      case RootPage(space) => doEdit(space.spaceThing)
      
      case _ => fail(s"editModel should be called from the Model's page, was called instead on ${state.currentPage}")
    }
  }
  
  /**
   * Edits the Model we are currently looking at.
   */
  def editModel(setters:(TThing[_], State) => State*)(state:State):State = {
    editModelGuts({click on "_thingEdit"}, setters:_*)(state)
  }
  
  /**
   * Opens the Advanced Editor on the TInstance we are currently looking at.
   */
  def advancedEditThing(setters:(TThing[_], State) => State*)(state:State):State = {
    editModelGuts({clickMenuItem(AdvancedEditItem)}, setters:_*)(state)
  }
  
  /**
   * Design a new Model.
   * 
   * @param ops Any number of operations to perform inside the Advanced Editor. Setting the Name is
   *   automatic, but everything else needs to be specified. 
   */
  def designAModel(modelBase:TInstance, setters:(TThing[_], State) => State*)(state:State):State = {
    // In order to save having to specify isModel = true everywhere, we just make it implicit
    // in calling this:
    val thing = modelBase.copy(isModel = true)
    spew(s"Designing Model ${thing.display}")
    // Choose "Design a Model" from the menu, and select the correct Model from the popup dialog:
    clickMenuItem(DesignModelItem)
    chooseModel(thing)(state)
    
    // Fill out the Model Designer, based on the specified properties:
    val page = ModelDesigner(thing, newlyCreated = true)
    // Note that this is *intentionally* duplicated: we don't yet know the TID for the model, so
    // we're getting it from the title:
    val partialTitle = page.msg("pageTitle", ("modelName" -> ""), ("prefix" -> page.prefix))
    // Note that we have to use include instead of be, because we haven't set the name yet, so it's
    // going to use the TID instead:
    eventually { pageTitle should include (partialTitle) }
    // Note the explicit assumption that the TID is at the end of the title here. That's suspicious, and
    // may not survive in the long run.
    val instanceTID = pageTitle.drop(partialTitle.length)
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
   * Add a Property inside he Model Designer.
   */
  def addExistingProperty[TPE <: TType](prop:TProp[TPE])(thing:TThing[_], state:State):State = {
    spew(s"Adding Property ${prop.display}")
    
    // TODO: can we merge the duplicate code between here and createProperty()? Probably.
    
    val instanceSection = getInstanceSection()
    
    def countEditors():Int = {
      val editors = instanceSection.findAll(xpath("./li[contains(@class, '_instanceEditor')]"))
      editors.size
    }
    
    // First, we need to look at the PropValueEditors already showing, since we'll
    // need to wait until another one is added:
    val nOrigEditors = countEditors()
    
    // What is currently showing?
    if (find("_addExistingProperty").isDefined) {
      // Don't need to do anything -- the dialog's already open
    } else if (find("_addExistingInstead").isDefined) {
      // The Create New Property dialog is open; switch boxes
      click on "_addExistingInstead"
      waitFor("_addExistingProperty")
    } else if (find("_addPropertyButton").isDefined) {
      click on "_addPropertyButton"
      waitFor("_createInstead")
    } else
      fail(s"Couldn't figure out where we are when trying to create Property ${prop.display}")
      
    // Pick the desired property...
    singleSel("_existingPropSelector").value = prop.tid
    // ... and actually add it:
    click on "_addExistingProperty"
    
    // Wait for the new PropValueEditor to be created:
    eventually { countEditors() should equal (nOrigEditors + 1) }
    
    state
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
    click on s"_coll${prop.coll.tid}"
    // ... the type...
    singleSel("_typeSelector").value = prop.tpe.tid
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
    waitForRendered()
    stateWithThing -> ThingPage(thingWithTID)
  }
}
