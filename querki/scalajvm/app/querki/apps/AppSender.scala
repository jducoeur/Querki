package querki.apps

import akka.actor._

import models.{Property, Thing, ThingState}

import querki.globals._
import querki.spaces.messages._
import querki.util.QuerkiActor

/**
 * Child Actor under an App, which sends its State to a requesting Space. This implements a very
 * primitive streaming protocol for the purpose, since States can get pretty large. This is much
 * more focused on being correct than either clever or efficient at this point. We expect that
 * most of this will become irrelevant once Akka Streams work remotely: what we really want is
 * simply an Actor-to-Actor stream that works across the cluster.
 * 
 * @author jducoeur
 */
class AppSender(e:Ecology, child:ActorRef, state:SpaceState) extends QuerkiActor(e) {
  import AppSender._
  
  val chunkSize = Config.getInt("querki.apps.chunksize", 10000)
  
  case class ChunkingState[T <: Thing](chunks:Seq[Seq[T]], current:Seq[T], currentSize:Int)
  
  // Note that this chunking algorithm intentionally always makes chunks a bit *larger* than the
  // chunkSize, so we don't hit weirdness if a single Thing is bigger than chunkSize. Thus, chunkSize
  // must be somewhat smaller than the actual limit.
  def makeChunks[T <: Thing](tmap:Map[OID, T]):Seq[Seq[T]] = {
    val endState = (ChunkingState(Seq.empty[Seq[T]], Seq.empty[T], 0) /: tmap.values) { (state, t) =>
      val ChunkingState(chunksIn, currentIn, currentSize) = state
      // If we're hit the cap of the current chunk, add it to the list:
      val (chunks, current) =
        if (currentSize > chunkSize)
          (chunksIn :+ currentIn, Seq.empty[T])
        else
          (chunksIn, currentIn)
      ChunkingState(chunks, current :+ t, currentSize + t.memsize)
    }
    
    if (endState.current.isEmpty)
      endState.chunks
    else
      endState.chunks :+ endState.current
  }
  
  lazy val allChunks:Map[Int, Seq[Thing]] = {
    val allChunks:Seq[Seq[Thing]] = 
      Seq(Seq(state.copy(spaceProps = Map.empty, things = Map.empty))) ++
      makeChunks(state.spaceProps) ++
      makeChunks(state.things)
    
    (Map.empty[Int, Seq[Thing]] /: allChunks.zipWithIndex) { (map, ichunk) =>
      val (things, i) = ichunk
      map + (i -> things)
    }
  }
  lazy val nChunks = allChunks.size
  
  def doReceive = {
    case Send => child ! AppStreamReady(self)
    
    case RequestChunk(sequence) => sender ! AppChunk(sequence, allChunks(sequence), (sequence == nChunks - 1))
    
    case StreamComplete => context.stop(self)
  }
}

object AppSender {
  case object Send
  
  /**
   * Ack from the Sender to the Loader, saying that we can start sending stream bits.
   */
  case class AppStreamReady(sender:ActorRef)
  
  /**
   * Request from the Loader to the Sender, asking for the specified chunk.
   */
  case class RequestChunk(sequence:Int)
  
  /**
   * A chunk of the App, consisting of some number of AppElements. The complete flag is
   * set if this is the last chunk.
   * 
   * Implicitly, the first Chunk is currently the SpaceState itself, shorn of Props and Things.
   */
  case class AppChunk(sequence:Int, elems:Seq[Thing], complete:Boolean)
  
  /**
   * Sent from the Loader to the Sender, to Ack that we're finished.
   */
  case object StreamComplete
  
  def props(e:Ecology, child:ActorRef, state:SpaceState) = Props(classOf[AppSender], e, child, state)
}
