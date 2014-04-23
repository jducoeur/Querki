package querki

import akka.actor.Props

import models.{OID, PType}

import querki.ecology._
import querki.values.QValue

package object uservalues {
  trait UserValues extends EcologyInterface {
    def RatingType:PType[_]
    
    /**
     * If this is a UserValue wrapping type, this returns the underlying type.
     */
    def getUserType(pt:PType[_]):Option[PType[_]]
    
    /**
     * To create a UserValuePersister Actor.
     */
    def userValuePersisterProps(spaceId:OID):Props
    
    /**
     * Given the user value for a Property and the Property's Type, get the UserValueWrapper QValue for it.
     */
    def wrapUserValue(uv:QValue, pt:PType[_], oldWrapperOpt:Option[QValue]):QValue
  }
}