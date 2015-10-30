package querki.streaming

/**
 * A single item to be streamed from a StreamSender to a StreamReceiver. Typically not implemented
 * directly, but done through a view bound. Must be serializable.
 */
trait StreamElement {
  /**
   * All StreamElements need a way to say how big the Chunks are.
   */
  def sizeAsChunk:Int
}

/**
 * Communications protocol between StreamSender and StreamReceiver. This is simplistic and inefficient,
 * but should do until we can replace this with Akka Streams. (Once those work remotely Actor-to-Actor.)
 * Note that the protocol implicitly assumes that both ends can afford to buffer the whole contents: the
 * point of the streaming protocol is solely to be able to send objects that are larger than the message
 * size limit. (Eg, Spaces.)
 * 
 * @author jducoeur
 */
private [streaming] object StreamMessages {
  /**
   * Signal to expect an incoming Stream.
   */
  case object StartStream
  
  /**
   * Request from the receiver to the sender for the specified chunk. May be sent multiple times, in
   * case of timeout.
   */
  case class RequestChunk(sequence:Int)
  
  /**
   * One chunk from the stream.
   * 
   * @param sequence The sequence ID of this element. Should be monotonically increasing from zero.
   * @param elems The actual data.
   * @param complete True iff this is the last chunk.
   */
  case class StreamChunk(sequence:Int, elems:Seq[AnyRef], complete:Boolean)
}
