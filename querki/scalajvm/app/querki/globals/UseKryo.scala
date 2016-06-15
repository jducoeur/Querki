package querki.globals

/**
 * Public marker trait, which all top-level persistence classes should use in order to
 * be serialized properly. 
 * 
 * Note that these classes, and all of their referenced classes, must use @Tag on all fields
 * to be serialized! Yes, this is a hassle, but it's necessary in order to get schema
 * evolution. (And the alternative is protobuf, which has the same problem but externally.)
 * 
 * This must be explicitly Serializable, so that it is unambiguously "more specific" than that.
 */
trait UseKryo extends java.io.Serializable