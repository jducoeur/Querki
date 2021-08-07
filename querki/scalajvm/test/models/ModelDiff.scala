package models

import ai.x.diff._

import querki.globals._
import querki.time.DateTime
import querki.types.{ModelTypeDefiner, ModeledPropertyBundle}
import querki.values._

/**
 * This builds on x.ai's diff library, providing DiffShow typeclass instances for the various
 * key types.
 *
 * Note that this is a work in progress. My objective was to get to the point where I could
 * diff two SpaceStates, and I have *not* gotten there yet, sadly. A lot of the bits and
 * pieces here work, but not all the way up there. I probably need to add a custom DiffShow
 * for SpaceState itself, I suspect.
 */
trait ModelDiff { self: EcologyMember =>
  implicit def dateTimeDiffShow: DiffShow[DateTime] = DiffShow.primitive(_.toString)
  lazy val emptyContext = EmptyContext(ecology)

  implicit def pTypeDiffShow = new DiffShow[PType[_]] {
    def show(d: PType[_]) = d.displayName

    def diff(
      l: PType[_],
      r: PType[_]
    ) =
      if (l.id == r.id)
        Identical(show(l))
      else
        Different(showChange(l, r))
  }

  def matchesType(
    l: PType[_],
    r: PType[_]
  ) = ElemValue.matchesType(l, r)

  /**
   * The DiffShow for QValue is pretty complex, since we're doing a proper ordered match and the diff
   * library doesn't contain that yet.
   *
   * TODO: abstract the guts of diff() out, and contribute it back to the library.
   */
  implicit def qvDiffShow = new DiffShow[QValue] {
    def showHead(d: QValue) = s"${d.cType.displayName}[${d.pType.displayName}]"

    def show(d: QValue) = DiffShow.constructor(
      showHead(d),
      d.cv.toList.map(elemV => ("", DiffShow.show(elemV)))
    )

    def diff(
      l: QValue,
      r: QValue
    ) = {
      // This comparison looks a bit odd. It's because PType comparison is generally by *reference*
      // equality, so it fails when comparing against rehydration:
      if (!l.matchesType(r.pType) && (l.pType.id != r.pType.id))
        Different(s"QValues have different PTypes -- ${l.pType.displayName} vs. ${r.pType.displayName}")
      else if (l.size != r.size)
        Different(s"QValues have different lengths -- ${l.size} vs. ${r.size}")
      else {
        val pairs = l.cv.zip(r.cv)
        val diffs = pairs.map { case (eleml, elemr) => DiffShow.diff(eleml, elemr) }
        if (diffs.exists { _.isInstanceOf[Different] }) {
          // Mismatch, so spell it out...
          val result = ((0, List.empty[String]) /: diffs) { (state, diff) =>
            val (nIdentical, strs) = state
            diff match {
              case Identical(_) => (nIdentical + 1, strs)
              case Different(elemd) => {
                if (nIdentical == 0)
                  (0, strs :+ elemd)
                else
                  (0, strs :+ s"... $nIdentical matches..." :+ elemd)
              }
            }
          }
          val strs =
            if (result._1 == 0)
              result._2
            else
              result._2 :+ s"... ${result._1} matches"
          Different(DiffShow.constructor(showHead(l), strs.map(("", _))))
        } else {
          Identical(show(l))
        }
      }
    }
  }

  type MT = ModelTypeDefiner#ModelType

  implicit def modelTypeDiffShow: DiffShow[MT] = new DiffShow[MT] {
    def show(d: MT) = d.displayName

    def diff(
      l: MT,
      r: MT
    ) =
      if (l.id == r.id)
        Identical(show(l))
      else
        Different(showChange(l, r))
  }

  // TODO: In principle, we should make this subtler, so that it can dive down into complex
  // elements. At the moment, a difference in a substructure will only be detected
  // and displayed coarse-grained.
  implicit def elemValueDiffShow: DiffShow[ElemValue] = new DiffShow[ElemValue] {

    def show(d: ElemValue) = {
      d.pType.debugRender(emptyContext)(d)
    }

    def diff(
      l: ElemValue,
      r: ElemValue
    ) = {
      (l, r) match {
        case (ElemValue(eleml, lmt: MT), ElemValue(elemr, rmt: MT)) => {
          val mtDiff = DiffShow.diff(lmt, rmt)
          val bundlel = eleml.asInstanceOf[ModeledPropertyBundle]
          val bundler = elemr.asInstanceOf[ModeledPropertyBundle]
          val elemDiff = DiffShow.diff(bundlel, bundler)
          (mtDiff, elemDiff) match {
            case (Identical(_), Identical(_)) => Identical(show(l))
            case (Different(_), _)            => mtDiff
            case (_, Different(_))            => elemDiff
          }
        }
        case _ => {
          if (l.pType.matches(l, r))
            Identical(show(l))
          else
            Different(showChange(l, r))
        }
      }
    }
  }

}
