package querki

import akka.actor.Props

import models.{OID, Property, PType}

import querki.ecology._
import querki.values.{QValue, SpaceState}

package object uservalues {
  
  val UserValuesTag = "User Values"
  
  trait UserValues extends EcologyInterface {
    /**
     * The Link from a User Value Property to its Summary Property.
     */
    def SummaryLink:Property[OID,OID]
    
    /**
     * The optional link from a Summary Property to the Property that it is summarizing.
     */
    def SummarizesPropertyLink:Property[OID,OID]
    
    /**
     * Is a given user allowed to have User Values?
     */
    def UserValuePermission:Property[OID,OID]
    
    /**
     * Set this flag to indicate that a Property is a User Value.
     */
    def IsUserValueFlag:Property[Boolean,Boolean]
    
    /**
     * To create a UserValuePersister Actor.
     */
    def userValuePersisterProps(spaceId:OID):Props
    
    /**
     * This is an efficient lookup for whether the specified Property is a UserValue.
     */
    def isUserValueProp(propId:OID)(implicit state:SpaceState):Boolean
  }
  
  trait Ratings extends EcologyInterface {
    def RatingProperty:Property[Int,Int]
  }
}