package querki

import akka.actor.Props

import models.{OID, Property, PType}

import querki.ecology._
import querki.values.{QValue, SpaceState}

package object uservalues {
  trait UserValues extends EcologyInterface {
    /**
     * The Link from a User Value Property to its Summary Property.
     */
    def SummaryLink:Property[OID,_]
    
    /**
     * Is a given user allowed to have User Values?
     */
    def UserValuePermission:Property[OID,_]
    
    /**
     * To create a UserValuePersister Actor.
     */
    def userValuePersisterProps(spaceId:OID):Props
    
    /**
     * This is an efficient lookup for whether the specified Property is a UserValue.
     */
    def isUserValueProp(propId:OID)(implicit state:SpaceState):Boolean
  }
}