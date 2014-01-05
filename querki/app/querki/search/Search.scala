package querki.search

import models._
import models.system._

import querki.ecology._

import querki.identity.User

import querki.values.RequestContext

// TODO: this should be an Ecot!!!
object Search {
  lazy val DisplayNameProp = getInterface[querki.basic.Basic].DisplayNameProp
  
  def search(rc:RequestContext, searchStr:String):Option[SearchResults] = {
    if (searchStr.length() < 3)
      None
    else if (rc.state.isEmpty)
      None
    else {
      implicit val space = rc.state.get
      
      val searchComp = searchStr.toLowerCase()
      val requester = rc.requester.getOrElse(User.Anonymous)
      
      def checkOne(t:Thing):Seq[SearchResult] = {
        // TODO: this isn't strictly correct -- ideally, this is a subtle combination of Can Read and Can View Source.
        // If the user Can View Source, then it's fine; otherwise, if he Can Read, we should really be checking the
        // *rendered* view of this Thing. But that's challenging, so this should probably become Can View Source for now.
        if (!space.canRead(requester, t.id))
          Seq()
        else if (t.unsafeDisplayName.toLowerCase().contains(searchComp)) {
          Seq(SearchResult(t, DisplayNameProp, 1.0, t.unsafeDisplayName, List(t.unsafeDisplayName.toLowerCase().indexOf(searchComp))))
        } else {
          // For now, we're only going to deal with Text types.
          // TODO: cope with Links, Tags, and things like that!
          // TODO: this currently only takes the first elem from the QValue; it should work on
          // all of them!
          val tResults:Iterable[Option[SearchResult]] = t.props.map { pair =>
            val (propId, propValue) = pair
            propValue.pType match {
              case textType:TextTypeBase => {
                val firstVal = propValue.firstAs(textType)
                firstVal.flatMap { qtext => 
                  val propComp = qtext.text.toLowerCase()
                  if (propComp.contains(searchComp)) {
                    
                    def matchLocs(start:Int):List[Int] = {
                      val i = propComp.indexOf(searchComp, start)
                      if (i == -1)
                        Nil
                      else
                        i :: matchLocs(i + 1)
                    }

                    Some(SearchResult(t, space.prop(propId).get, 0.5, qtext.text, matchLocs(0)))
                  }
                  else
                    None
                }
              }
              case _ => None
            }
          }
          tResults.flatten.toSeq
        }
      }
      
      val allResults = space.everythingLocal.map(checkOne(_)).flatten.toSeq
    
      Some(SearchResults(searchStr, allResults))      
    }
  }
}