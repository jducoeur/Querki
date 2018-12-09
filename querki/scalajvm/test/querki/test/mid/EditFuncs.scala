package querki.test.mid

import cats.effect.IO

import autowire._

import querki.data._
import querki.editing._
import querki.editing.EditFunctions._
import querki.globals._

trait EditFuncs { self: ClientFuncs with ApiFuncs with ThingFuncs =>
  
  /**
   * Given a newly-created Thing, put it into the current Space.
   */
  def addNewThing(info: ThingInfo): TestOp[Unit] = WorldState.updateCurrentSpace { space =>
    space.copy(things = (space.things + (info.oid -> ThingTestState(info))))
  }
  
  /**
   * Updates the current "client-side" knowledge about the specified Thing.
   */
  def updateThing(thingId: TID): TestOp[Unit] = {
    for {
      info <- getThingInfo(thingId)
      _ <- WorldState.updateCurrentSpace { space =>
        space.copy(things = (space.things + (info.oid -> ThingTestState(info))))
      }
    }
      yield ()
  }

  /**
   * The raw, underlying call to create a new Thing. Use this when you want to set initialProps,
   * but that's not a good representation of how things usually work in the client.
   */
  def createThing(modelId: TID, initialProps: Seq[PropertyChange] = Seq.empty): TestOp[TID] = {
    for {
      info <- TestOp.client { _[EditFunctions].create(modelId, initialProps).call() }
      _ <- addNewThing(info)
    }
      yield info.oid
  }
  
  def propPath(thingId: TID, propId: TID): String = {
    // Idiotic, but I am suffering for mistakes made long ago:
    def chop(id: TID): String = id.underlying.drop(1)
    s"v-${chop(propId)}-${chop(thingId)}"
  }

  /**
   * Calls the server to change the specified Thing's Property value. Throws an Exception if the result indicates
   * that it was not saved, unless permitErrors is true.
   */
  def changeProp(thingId: TID, v: SaveablePropVal, permitErrors: Boolean = false): TestOp[Unit] = {
    for {
      response <- TestOp.client { 
        _[EditFunctions].alterProperty(thingId, ChangePropertyValue(propPath(thingId, v.propId), v.v.toSave)).call()
      }
      _ = if ((response == PropertyNotChangedYet) && !permitErrors) throw new Exception(s"Got false when trying to set $thingId.$v")
      _ <- updateThing(thingId)
    }
      yield ()
  }
  
  /**
   * Change the given Thing to have the given Props.
   * 
   * Note that this does *not* correspond to a single real-world operation; rather, it represents
   * a series of small edits to the Thing. Basically, it matches the user having an open editor,
   * changing Properties one at a time.
   * 
   * Normally, if the change is rejected by the server, that becomes an Exception test-side. If you
   * want that to be legal (eg, you are testing validation errors), set permitErrors to true.
   */
  def changeProps(thingId: TID, vs: List[SaveablePropVal], permitErrors: Boolean = false): TestOp[Unit] = {
    vs match {
      case v :: tl => {
        for {
          result <- changeProp(thingId, v)
          recurse <- changeProps(thingId, tl, permitErrors)
        }
          yield recurse
      }
      case Nil => TestOp.unit
    }
  }

  /**
   * Creates a new Thing from the specified Model, and then sets the given values.
   * 
   * Note that this is multiple operations from the server's POV, and emulates the usual
   * creation process.
   */
  def makeUnnamedThing(modelId: TID, vs: SaveablePropVal*): TestOp[TID] = {
    for {
      thingId <- createThing(modelId)
      result <- changeProps(thingId, vs.toList)
    }
      yield thingId
  }
  def makeUnnamedThing(model: BasicThingInfo, vs: SaveablePropVal*): TestOp[TID] =
    makeUnnamedThing(model.oid, vs:_*)
  
  /**
   * The typical process for creating a Thing. This simulates creating it, setting the Name,
   * and then setting other Properties.
   */
  def makeThing(modelId: TID, name: String, vs: SaveablePropVal*): TestOp[TID] = {
    for {
      thingId <- makeUnnamedThing(modelId)
      std <- fetchStandardThings()
      _ <- changeProp(thingId, std.basic.displayNameProp.oid :=> "First Thing")
      _ <- changeProps(thingId, vs.toList)
    }
      yield thingId
  }
  
  implicit class RichPropId(propId: TID) {
    def :=>[T](v: T)(implicit saveable: Saveable[T]): SaveablePropVal = {
      SaveablePropVal(propId, saveable.toSaveable(v))    
    }
  }
}
