package querki.test

import querki.ecology._

/**
 * This is a simple but relatively realistic Space, to use in tests that want
 * chewier data. It will get gradually enhanced, but do a full retest when you do so.
 */
class CDSpace(implicit ecologyIn: Ecology) extends CommonSpace {
  val Basic = ecology.api[querki.basic.Basic]
  val DisplayName = Basic.DisplayNameProp

  val artistModel = new SimpleTestThing("Artist", DisplayName())
  val exemplar = new TestProperty(LinkType, Optional, "Exemplar", Links.LinkModelProp(artistModel))
  val genreModel = new SimpleTestThing("Genre")

  val genres = new TestProperty(Tags.NewTagSetType, QSet, "Genres")

  val eurythmics = new TestThing("Eurythmics", artistModel, genres("Rock"))

  val tmbg =
    new TestThing("They Might Be Giants", artistModel, genres("Rock", "Weird"), DisplayName("They Might Be Giants"))

  val blackmores =
    new TestThing("Blackmores Night", artistModel, genres("Rock", "Folk"), DisplayName("Blackmores Night"))
  val whitney = new TestThing("Whitney Houston", artistModel, genres("Pop"))
  val weirdAl = new TestThing("Weird Al", artistModel, genres("Parody"))
  val bok = new TestThing("Gordon Bok", artistModel, genres("Folk"))

  val folk = new TestThing("Folk", genreModel, exemplar(blackmores))

  val artistsProp = new TestProperty(LinkType, QSet, "Artists", Links.LinkModelProp(artistModel))

  val albumModel = new SimpleTestThing("Album", artistsProp())

  val firesAtMight = new TestThing("Fires at Midnight", albumModel, artistsProp(blackmores))
  val ghostOfARose = new TestThing("Ghost of a Rose", albumModel, artistsProp(blackmores))
  val shadowOfTheMoon = new TestThing("Shadow of the Moon", albumModel, artistsProp(blackmores))

  val flood = new TestThing("Flood", albumModel, artistsProp(tmbg))
  val factoryShowroom = new TestThing("Factory Showroom", albumModel, artistsProp(tmbg))

  val runningWithScissors = new TestThing("Running with Scissors", albumModel, artistsProp(weirdAl))
  val mandatoryFun = new TestThing("Mandatory Fun", albumModel, artistsProp(weirdAl))

  val randomCollection = new TestThing("Classical Randomness", albumModel)

  new TestThing("Be Yourself Tonight", albumModel, artistsProp(eurythmics))
  new TestThing("Touch", albumModel, artistsProp(eurythmics))
  new TestThing("We Too Are One", albumModel, artistsProp(eurythmics))

  val favoriteArtistsProp = new TestProperty(LinkType, QList, "Favorite Artists", Links.LinkModelProp(artistModel))

  val interestingArtistsProp =
    new TestProperty(LinkType, QList, "Interesting Artists", Links.LinkModelProp(artistModel))
  val otherArtistsProp = new TestProperty(Tags.NewTagSetType, QSet, "Other Artists", Links.LinkModelProp(artistModel))

  val faveDisplayProp = new TestProperty(TextType, ExactlyOne, "Show Favorites")

  val faves = new SimpleTestThing(
    "My Favorites",
    favoriteArtistsProp(tmbg, blackmores),
    interestingArtistsProp(eurythmics),
    otherArtistsProp("Weird Al"),
    faveDisplayProp("My favorite bands are: [[Favorite Artists -> _bulleted]]")
  )
}
