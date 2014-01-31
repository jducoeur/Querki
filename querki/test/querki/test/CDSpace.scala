package querki.test

import querki.ecology._

/**
 * This is a simple but relatively realistic Space, to use in tests that want
 * chewier data. It will get gradually enhanced, but do a full retest when you do so.
 */
class CDSpace(implicit ecologyIn:Ecology) extends CommonSpace {
  val artistModel = new SimpleTestThing("Artist")
    
  val eurythmics = new TestThing("Eurythmics", artistModel)
  val tmbg = new TestThing("They Might Be Giants", artistModel)
  val blackmores = new TestThing("Blackmores Night", artistModel)
  val whitney = new TestThing("Whitney Houston", artistModel)
    
  val artistsProp = new TestProperty(LinkType, QSet, "Artists", Links.LinkModelProp(artistModel))
    
  val albumModel = new SimpleTestThing("Album", artistsProp())
    
  val firesAtMight = new TestThing("Fires at Midnight", albumModel, artistsProp(blackmores))
  val ghostOfARose = new TestThing("Ghost of a Rose", albumModel, artistsProp(blackmores))
  val shadowOfTheMoon = new TestThing("Shadow of the Moon", albumModel, artistsProp(blackmores))
    
  val flood = new TestThing("Flood", albumModel, artistsProp(tmbg))
  val factoryShowroom = new TestThing("Factory Showroom", albumModel, artistsProp(tmbg))
  
  val randomCollection = new TestThing("Classical Randomness", albumModel)
    
  new TestThing("Be Yourself Tonight", albumModel, artistsProp(eurythmics))
  new TestThing("Touch", albumModel, artistsProp(eurythmics))
  new TestThing("We Too Are One", albumModel, artistsProp(eurythmics))
  
  val favoriteArtistsProp = new TestProperty(LinkType, QList, "Favorite Artists", Links.LinkModelProp(artistModel))
  
  new SimpleTestThing("My Favorites", favoriteArtistsProp(tmbg, blackmores))
}
