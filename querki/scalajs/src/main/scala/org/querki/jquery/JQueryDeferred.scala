package org.querki.jquery

import scala.scalajs.js

trait JQueryPromise extends js.Object {
  /**
   * Add handlers to be called when the Deferred object is either resolved or rejected.
   */
  def always(alwaysCallbacks: js.Function*): JQueryDeferred = js.native
  /**
   * Add handlers to be called when the Deferred object is resolved.
   */
  def done(doneCallbacks: js.Function*): JQueryDeferred = js.native
  /**
   * Add handlers to be called when the Deferred object is rejected.
   */
  def fail(failCallbacks: js.Function*): JQueryDeferred = js.native
  
  // TODO: add `then`(). Don't bother with pipe() -- it is deprecated.
}

trait JQueryDeferred extends JQueryPromise {
  /**
   * Call the progressCallbacks on a Deferred object with the given args.
   */
  def notify(args: js.Any*): JQueryDeferred = js.native
  
  /**
   * Call the progressCallbacks on a Deferred object with the given context and args.
   */
  def notifyWith(context: js.Object, args: js.Array[_]): JQueryDeferred = js.native
  /**
   * Add handlers to be called when the Deferred object generates progress notifications.
   * 
   * Each parameter can be either a Function *or* an Array of Functions.
   */
  def progress(progressCallbacks: js.Any*): JQueryDeferred = js.native
  /**
   * Reject a Deferred object and call any failCallbacks with the given args.
   */
  def reject(args: js.Any*): JQueryDeferred = js.native
  /**
   * Reject a Deferred object and call any failCallbacks with the given context and args.
   */
  def rejectWith(context: js.Object, args: js.Array[Any]): JQueryDeferred = js.native
  /**
   * Resolve a Deferred object and call any doneCallbacks with the given args.
   */
  def resolve(args: js.Any*): JQueryDeferred = js.native
  /**
   * Resolve a Deferred object and call any doneCallbacks with the given context and args.
   */
  def resolveWith(context: js.Object, args: js.Array[Any]): JQueryDeferred = js.native
  /**
   * Determine the current state of a Deferred object.
   */
  def state(): String = js.native
}
