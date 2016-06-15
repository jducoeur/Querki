package querki

/**
 * Types relating to the use of Akka Persistence (and Kryo) within Querki. Any module
 * that has any persistent actors should include querki.persistence._ to define their
 * messages.
 */
package object persistence {
  
  /**
   * In all UseKryo types *and* anything referenced by them that will be persisted, you *must*
   * set @KryoTag on *all* fields to be persisted!
   * 
   * @KryoTag takes one Int parameter, which is the permanent ID of this field within this type.
   * Start with 1 and increment from there. *NEVER* change this, *NEVER* reuse numbers, and for
   * the time being (at least until Kryo 3.0.4) *NEVER* delete a field!
   * 
   * Note that, in order to make this work right, we need to meta-annotate this so that the
   * compiler knows how to use it.
   */
  type KryoTag = com.esotericsoftware.kryo.serializers.TaggedFieldSerializer.Tag @scala.annotation.meta.field
  
}
