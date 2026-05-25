package querki.photos

import akka.actor._
import akka.event.LoggingReceive

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO
import org.imgscalr.Scalr

import java.security.MessageDigest
import java.math.BigInteger
import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials}
import com.amazonaws.regions.{Regions}
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{AccessControlList, GroupGrantee, ObjectMetadata, Permission, PutObjectRequest}
import org.querki.requester.Requester
import models.{MIMEType}
import querki.core.QLText
import querki.globals._
import querki.photos.PhotoUploadMessages.PhotoUploadMetadata
import querki.session.messages.ChangeProps2
import querki.spaces.messages.{BeginProcessingPhoto, ImageComplete, SpaceSubsystemRequest, ThingError, ThingFound}
import querki.streaming.UploadActor
import querki.time.DateTime
import querki.types.SimplePropertyBundle
import querki.util.{Config}
import querki.values.{QLContext, SpaceState}

class PhotoUploadActor(
  e: Ecology,
  state: SpaceState,
  router: ActorRef
) extends Actor
     with Requester
     with UploadActor[PhotoUploadMetadata]
     with EcologyMember {

  import PhotoUploadMessages._

  implicit val ecology: Ecology = e

  lazy val Core = interface[querki.core.Core]
  lazy val PhotosInternal = interface[PhotosInternal]
  lazy val QL = interface[querki.ql.QL]
  lazy val TimeProvider = interface[querki.time.TimeProvider]

  object AWS {
    def get(name: String) = Config.getString("querki.aws." + name)
    val username = get("username")
    val accessKeyId = get("accessKeyId")
    val secretAccessKey = get("secretAccessKey")
    val bucket = get("bucket")
  }

  var _mimeType: Option[String] = None
  def mimeType = _mimeType.getOrElse(MIMEType.JPEG)

  // The guts of the upload processing, called after we have built the input stream
  def processBuffer(
    metadata: PhotoUploadMetadata,
    metadataSender: ActorRef
  ): Unit = {
    val PhotoUploadMetadata(rc, propId, thingId) = metadata

    spewIfEnabled(s"UploadDone; type is $mimeType")
    // DO NOT CHECK IN -- this should only be uncommented for debugging:
    // logBuffer()
    val originalImage = ImageIO.read(uploadedStream)

    if (originalImage == null) {
      throw new Exception(
        s"Null result from ImageIO.read(); suggests that it can't find an ImageReader for MIME type $mimeType"
      )
    }

    spewIfEnabled(s"Original Image size is ${originalImage.getWidth()}x${originalImage.getHeight()}")

    implicit val s = state

    val prop = s.prop(propId).getOrElse(throw new Exception(s"Attempting to upload unknown Property $propId"))
    val thing = s.anything(thingId).getOrElse(throw new Exception(s"Attempting to upload unknown Property $propId"))
    val oldValueOpt = thing.getPropOpt(propId).map(_.v)

    val maxSize = prop.getFirstOpt(PhotosInternal.PreferredImageSizeProp).getOrElse(1000)

    if (originalImage == null) {
      // TODO: is this happening any more? ImageIO.read() was occasionally returning null, which I
      // believe was a symptom of the messages from PhotoController to here getting out of order,
      // because we weren't composing Futures properly. I *think* that is fixed...
      logError("OriginalImage is null! WTF?")
    }

    // For the time being, we are presuming that PNGs are screenshots, and should just be left alone:
    val shouldResize = !(mimeType == MIMEType.PNG)

    val digester = MessageDigest.getInstance("SHA-1")

    def resizeImage(size: Int): (ByteArrayOutputStream, String, Int, Int) = {
      // Using QUALITY is a bit slower, but I find that a few JPEGs wind up badly messed-up without it:
      val resizedImage =
        if (size > 0)
          Scalr.resize(originalImage, Scalr.Method.QUALITY, size)
        else
          originalImage
      val outputStream = new ByteArrayOutputStream()
      val ioType = mimeType match {
        case MIMEType.PNG => "png"
        case _            => "jpeg"
      }
      val fileSuffix = mimeType match {
        case MIMEType.PNG  => ".png"
        case MIMEType.JPEG => ".jpg"
        case _             => ""
      }
      ImageIO.write(resizedImage, ioType, outputStream)
      resizedImage.flush()
      val digest = digester.digest(outputStream.toByteArray)
      val digestInt = new BigInteger(1, digest)
      val filename = state.id.toString + "/" + digestInt.toString(16) + fileSuffix
      (outputStream, filename, resizedImage.getHeight(), resizedImage.getWidth())
    }

    val (outputStream, filename, height, width) = resizeImage(if (shouldResize) maxSize else 0)
    val (thumbnailOutputStream, thumbnailFilename, thumbHeight, thumbWidth) = resizeImage(100)

    // Send it off to Amazon...
    val credentials = new BasicAWSCredentials(AWS.accessKeyId, AWS.secretAccessKey)
    val s3client =
      AmazonS3Client.builder()
        .withCredentials(new AWSStaticCredentialsProvider(credentials))
        .withRegion(Regions.US_WEST_2)
        .build()
    spewIfEnabled(s"Got the s3client stood up")

    // TODO: EEEEVIL! This is currently synchronous, and presumably takes A Long Time. We need to either
    // find an async version of these entry points, or do these operations in an Actor that uses a
    // different Dispatcher, to avoid blocking the world! See whether TransferManager can help:
    // http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/s3/transfer/TransferManager.html
    def uploadToS3(
      stream: ByteArrayOutputStream,
      name: String
    ) =
      try {
        val metadata = new ObjectMetadata()
        metadata.setContentType(mimeType)
        // Our photographs deliberately have nigh-infinite expiration. 1 year is the max, per HTTP spec:
        metadata.setExpirationTime(DateTime.now.plusYears(1).toDate())
        // For the time being, all photos are world-readable; we only have security-through-obscurity
        // for now.
        // TODO: if the Thing is private, use Amazon's Secure Token Service to grant permission at read time.
        // Note, however, that this will make some accesses *MUCH* slower, since it will require roundtrips
        // to Amazon!
        val acl = new AccessControlList()
        acl.grantPermission(GroupGrantee.AllUsers, Permission.Read)
        val putRequest = new PutObjectRequest(
          AWS.bucket,
          name,
          new ByteArrayInputStream(stream.toByteArray()),
          metadata
        )
        putRequest.setAccessControlList(acl)
        val putResult = s3client.putObject(putRequest)
      } catch {
        case e: Throwable => {
          logError(s"Error while trying to upload $name", e)
          throw e
        }
      }

    uploadToS3(outputStream, filename)
    spewIfEnabled(s"Uploaded the main image")
    uploadToS3(thumbnailOutputStream, thumbnailFilename)
    spewIfEnabled(s"Uploaded the thumbnail")

    // Okay -- now, rebuild the actual QValue
    val rawBundle = SimplePropertyBundle(
      PhotosInternal.ImageHeightProp(height),
      PhotosInternal.ImageWidthProp(width),
      PhotosInternal.ImageMIMETypeProp(MIMEType.JPEG),
      PhotosInternal.ImageFilenameProp(filename),
      // TODO: this really should be based on the timestamp in the photo data, if any:
      PhotosInternal.ImageTimestampProp(DateTime.now),
      PhotosInternal.ImageSizeProp(outputStream.toByteArray().length),
      PhotosInternal.ImageThumbnailFilenameProp(thumbnailFilename),
      PhotosInternal.ImageThumbnailHeightProp(thumbHeight),
      PhotosInternal.ImageThumbnailWidthProp(thumbWidth)
    )
    val elem = PhotosInternal.PhotoType(rawBundle)
    val qv = oldValueOpt match {
      case Some(oldValue) => {
        // Note that this returns a tuple of the new QValue and the old value, if any
        val result = oldValue.append(elem)
        // If there was a previous value, delete it from S3
        // TODO: we are eventually going to need to figure out how to deal with photos that
        // are copied in multiple Things, so we should only delete them when the last copy is removed.
        // I suspect we'll need to use metadata to reference-count:
        for {
          oldPhotoElem <- result._2
          oldBundle <- oldPhotoElem.getOpt(PhotosInternal.PhotoType)
          oldFilename <- oldBundle.getFirstOpt(PhotosInternal.ImageFilenameProp)
          oldThumbFilename <- oldBundle.getFirstOpt(PhotosInternal.ImageThumbnailFilenameProp)
        } yield {
          logTrace(s"Removing ${oldFilename.text} and ${oldThumbFilename.text}")
          s3client.deleteObject(AWS.bucket, oldFilename.text);
          s3client.deleteObject(AWS.bucket, oldThumbFilename.text)
        }

        result._1
      }
      case None => prop.cType.makePropValue(Some(elem), PhotosInternal.PhotoType)
    }

    def finish() = {
      // Okay, start shutting down...
      originalImage.flush()
      self ! ImageComplete
    }

    //      logTrace(s"About to actually update the Space -- the QValue is $qv")
    (router ? SpaceSubsystemRequest(
      rc.requesterOrAnon,
      state.id,
      ChangeProps2(thing.id.toThingId, Map((propId -> qv)))
    )).foreach { response =>
      response match {
        case ThingFound(thingId, newState) => {
          // Okay, we're successful. Send the Wikitext for thumbnail of the new photo back to the Client:
          val lastElem = qv.cv.last
          val processFut = QL.process(
            QLText("[[_thumbnail]]"),
            QLContext(Core.ExactlyOne(lastElem), Some(rc), TimeProvider.qlEndTime)(newState, ecology)
          )
          loopback(processFut).map { wikified =>
            metadataSender ! PhotoInfo(wikified)
            finish()
          }
        }
        case ThingError(error, stateOpt) => {
          metadataSender ! PhotoFailed
          finish()
        }
      }
    }
  }

  // TODO: break this out into its own object, and filter it on the TRACE level of logging for that object, so we
  // can turn it on and off in a more fine-grained way:
  def logBuffer() = {
    logTrace(s"Dumping photo to log for debugging")

    // We need to make sure we clean up after ourselves. Yes, it's a horrible mutable Java API:
    uploadedStream.mark(1000000)

    def printBytes(howMany: Int): String = {
      (1 to howMany).map { _ =>
        val byte = uploadedStream.read()
        if (byte >= 0)
          f"$byte%2x"
        else
          // read() returns -1 when we've hit EOF, so just ignore that:
          ""
      }.mkString(" ")
    }

    // Just keep telling myself that when in Java APIs, do as the Javoids do:
    var i: Int = 0
    while (uploadedStream.available() > 0) {
      logTrace(f"$i%8x: ${printBytes(16)}")
      i += 16
    }

    uploadedStream.reset()
  }

  /**
   * Quick and dirty method for grouping the spews in here.
   *
   * TODO: remove this in favor of more conventional control of the TRACE log level.
   */
  def spewIfEnabled(msg: String): Unit = {
    logTrace(msg)
  }

  override def receive = LoggingReceive(handleChunks.orElse {

    case BeginProcessingPhoto(_, spaceId, tpe) => {
      spewIfEnabled(s"Starting to process photo of type $tpe")
      _mimeType = tpe
    }

    case ImageComplete => {
      context.stop(self)
    }
  })
}

object PhotoUploadActor {

  def actorProps(
    ecology: Ecology,
    state: SpaceState,
    router: ActorRef
  ): Props = Props(classOf[PhotoUploadActor], ecology, state, router)
}
