import querki.values.RequestContext

package object controllers {
  type PlayRequestContext = PlayRequestContextFull[_]
  implicit def prc2rc(prc: PlayRequestContext): RequestContext = prc.rc
}
