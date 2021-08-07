package querki.logic

import querki.ecology.QuerkiEcot

import querki.values.ElemValue

trait YesNoUtils { self: QuerkiEcot =>
  lazy val True = YesNoType(true)
  lazy val False = YesNoType(false)

  implicit def boolean2YesNo(raw: Boolean): ElemValue = {
    if (raw)
      True
    else
      False
  }

  // This is *begging* to crash, until YesNoType moves into Core!
  implicit def boolean2YesNoQValue(raw: Boolean): QValue = {
    Core.ExactlyOne(raw)
  }

  def toBoolean(typed: QValue): Boolean = {
    if (typed.pType == YesNoType)
      typed.firstAs(YesNoType).getOrElse(false)
    else
      false
  }
}
