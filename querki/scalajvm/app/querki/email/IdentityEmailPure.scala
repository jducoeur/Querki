package querki.email

/**
 * This holds the pure state-transformation code inside IdentityEmailCore.
 *
 * Note that IdentityEmail is a relatively *impure* Actor -- much of its activity is side-effects and persistence --
 * so this does less than some of the Pure traits.
 */
trait IdentityEmailPure {}
