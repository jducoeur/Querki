package querki

import querki.globals._

package object tools {

  trait Profiler extends EcologyInterface {

    /**
     * Get the handle for a profiler that will be used many times in the code.
     *
     * @param name The name of this particular profiler, which should be unique -- use something based on the
     *   class name. This is used to turn profilers on an off in config. If this name is *not* defined as true
     *   in config, under the "profile" node, the returned handle will be empty: you can use it as normal, but
     *   it will do nothing, as efficiently as possible. (This is, in practice, very cheap.)
     */
    def createHandle(name: String): ProfileHandle
  }

  /**
   * A single "profiler". You generally create one of these in an Ecot (or if necessary, an object), and
   * then instantiate it each time you enter your critical section.
   *
   * Why are we doing our own profiling, instead of using a third-party tool? Partly because the good tools
   * are expensive, but mostly because they aren't nearly so drop-in when dealing with asynchronous code,
   * and we are heavy in async.
   *
   * IMPORTANT: this is strictly focused on profiling *latency*. It does not measure CPU or memory in any way.
   * This is all about figuring out why the user is getting slow response.
   */
  trait ProfileHandle {

    /**
     * Start a single timing run, and get an instance for it. Call stop() on that instance when complete,
     * and let go of it. Use this if the code is asynchronous; use profile() (which is easier) for synchronous
     * calls.
     */
    def start(namePlus: String = ""): ProfileInstance

    /**
     * Do a synchronous timing of the given code. This basically encapsulates the start/stop pair, and is
     * the easy way to do things if the code is synchronous. Just put the code to time in a block, and go.
     */
    def profile[T](f: => T): T

    /**
     * Similar to profile, but groks Futures. This profiles until the returned Future is *complete*.
     */
    def profileFut[T](f: => Future[T]): Future[T]

    def profileAs[T](namePlus: String)(f: => T): T
  }

  /**
   * Represents a single instance of a ProfileHandle, which you create for a single timing run.
   */
  trait ProfileInstance {

    /**
     * Finish this particular timing run.
     */
    def stop(): Unit
  }
}
