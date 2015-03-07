package querki.ecology

abstract class ClientEcot(ecologyIn:Ecology) extends EcotImpl {
  
  // Note that this cannot, sadly, be a val, because it is needed in Ecot's constructor:
  implicit def ecology = ecologyIn

}