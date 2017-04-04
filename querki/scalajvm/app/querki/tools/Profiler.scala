package querki.tools

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success}

import querki.globals._

import querki.ecology._
import querki.time._
import querki.util.Config

class ProfilerEcot(e:Ecology) extends QuerkiEcot(e) with Profiler {
  
  override def term() = {
    if (!profiles.isEmpty) {
      // We've done some profiling, so print out the results.
      QLog.info("Profile results:")
      val allProfiles = profiles.values.toSeq.sortBy(_.name)
      allProfiles.foreach { profile =>
        QLog.info(s"${profile.name}: ${profile.runs} runs, averaging ${(profile.time.getMillis / profile.runs)} ms, total ${profile.time.getMillis}")
      }
    }
  }

  private case class ProfileRecord(name:String, runs:Int, time:Duration)
  
  // IMPORTANT: this is a synchronous data structure, from Scala's core libraries! This is to allow
  // multi-threaded updates. This is a bit of a cheat in our usually Actor-centric architecture, to
  // avoid having Actor messages from profiling overwhelm what we're trying to measure.
  private val profiles = TrieMap.empty[String, ProfileRecord]

  def createHandle(name:String):ProfileHandle = {
    if (Config.getBoolean(s"querki.profile.$name", false))
      ProfileHandleImpl(this, name)
    else
      NullProfileHandle
  }
  
  private [tools] def recordRun(instance:ProfileInstanceImpl):Unit = {
    val name = instance.name
    val time = new Duration(instance.startTime, DateTime.now)
    // TODO: this should be replaced with profiles.getOrElseUpdate --
    profiles.get(name) match {
      case Some(record) => {
        profiles += (name -> record.copy(runs = record.runs + 1, time = record.time + time))
      }
      case None => profiles += (name -> ProfileRecord(name, 1, time))
    }
  }
}

private [tools] case object NullProfileHandle extends ProfileHandle {
  def start(namePlus:String = "") = NullProfileInstance
  def profileAs[T](namePlus:String)(f: => T):T = f
  def profile[T](f: => T):T = f
  def profileFut[T](f: => Future[T]):Future[T] = f
}

private [tools] case object NullProfileInstance extends ProfileInstance {
  def stop() = {}
}

private [tools] case class ProfileHandleImpl(ecot:ProfilerEcot, name:String) extends ProfileHandle {
  def start(namePlus:String = ""):ProfileInstance = {
    new ProfileInstanceImpl(this, DateTime.now, namePlus)
  }
  
  def profileAs[T](namePlus:String)(f: => T):T = {
    val instance = start(namePlus)
    try {
      f
    } finally {
      instance.stop()
    }
  }
  
  def profile[T](f: => T):T = profileAs("")(f)
  
  def profileFut[T](f: => Future[T]):Future[T] = {
    val instance = start("")
    try {
      val fut = f
      fut.onComplete {
        case Success(_) => instance.stop()
        case Failure(_) => instance.stop()
      }
      fut
    } catch {
      case th:Throwable => { instance.stop(); throw th }
    }
  }
}

private [tools] case class ProfileInstanceImpl(handle:ProfileHandleImpl, startTime:DateTime, namePlus:String) extends ProfileInstance {
  def stop() = handle.ecot.recordRun(this)
  
  def name = handle.name + namePlus
}
