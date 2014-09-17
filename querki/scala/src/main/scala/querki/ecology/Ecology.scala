package querki.ecology

import scala.reflect.ClassTag

/**
 * This is a pure marker trait. All "interfaces" exposed through the Ecology *must* have this as their
 * first trait linearly. (Usually, it will be the only thing that an exposed interface extends, but
 * that is not required.)
 */
trait EcologyInterface
