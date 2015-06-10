package querki.photos

import akka.actor._
import akka.event.LoggingReceive

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.ImageIO
import org.imgscalr.Scalr

import java.security.MessageDigest
import java.math.BigInteger

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.regions.{Region, Regions}
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{AccessControlList, GroupGrantee, ObjectMetadata, Permission, PutObjectRequest}

import models.{MIMEType, OID}

import querki.ecology._
import querki.spaces.messages.{BeginProcessingPhoto, ImageComplete}
import querki.time.DateTime
import querki.types.SimplePropertyBundle
import querki.util.{Config, QLog}
import querki.values.{ElemValue, SpaceState}

class PhotoUploadActor(val ecology:Ecology, state:SpaceState) extends Actor with EcologyMember {
  
  import PhotoUploadActor._
  import PhotoUploadMessages._
  
  lazy val Core = interface[querki.core.Core]
  lazy val PhotosInternal = interface[PhotosInternal]
  
  object AWS {
    def get(name:String) = Config.getString("querki.aws." + name)
    val username = get("username")
    val accessKeyId = get("accessKeyId")
    val secretAccessKey = get ("secretAccessKey")
    val bucket = get("bucket")
  }
  
  var chunkBuffer:Vector[Byte] = Vector.empty
  
  var _mimeType:Option[String] = None
  def mimeType = _mimeType.getOrElse(MIMEType.JPEG)
  
  def receive = LoggingReceive {
    
    case BeginProcessingPhoto(req, spaceId, tpe) => {
      _mimeType = tpe
    }
    
    case PhotoChunk(chunk) => {
      chunkBuffer = chunkBuffer ++ chunk
      QLog.spew(s"Actor got ${chunk.length} bytes")
    }
    
    case UploadDone(oldValueOpt, prop) => {
      QLog.spew(s"UploadDone -- got ${chunkBuffer.size} bytes; type is $mimeType")
      val inputStream = new ByteArrayInputStream(chunkBuffer.toArray)
      val originalImage = ImageIO.read(inputStream)
      
      QLog.spew(s"Original Image size is ${originalImage.getWidth()}x${originalImage.getHeight()}")
      
      implicit val s = state
      
      val maxSize = prop.getFirstOpt(PhotosInternal.PreferredImageSizeProp).getOrElse(1000)
      
      if (originalImage == null) {
        // TODO: is this happening any more? ImageIO.read() was occasionally returning null, which I
        // believe was a symptom of the messages from PhotoController to here getting out of order,
        // because we weren't composing Futures properly. I *think* that is fixed...
        QLog.error("OriginalImage is null! WTF?")
      }
      
      // For the time being, we are presuming that PNGs are screenshots, and should just be left alone:
      val shouldResize = !(mimeType == MIMEType.PNG) 
      
      val digester = MessageDigest.getInstance("SHA-1")
      def resizeImage(size:Int):(ByteArrayOutputStream, String, Int, Int) = {
        // Using QUALITY is a bit slower, but I find that a few JPEGs wind up badly messed-up without it:
        val resizedImage =
          if (size > 0)
            Scalr.resize(originalImage, Scalr.Method.QUALITY, size)
          else
            originalImage
        val outputStream = new ByteArrayOutputStream()
        val ioType = mimeType match {
          case MIMEType.PNG => "png"
          case _ => "jpeg"
        }
        val fileSuffix = mimeType match {
          case MIMEType.PNG => ".png"
          case MIMEType.JPEG => ".jpg"
          case _ => ""
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
      val s3client = new AmazonS3Client(credentials)
      s3client.setRegion(Region.getRegion(Regions.US_WEST_2))
      def uploadToS3(stream:ByteArrayOutputStream, name:String) = {
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
            metadata)
        putRequest.setAccessControlList(acl)
        val putResult = s3client.putObject(putRequest)
      }
      uploadToS3(outputStream, filename)
      uploadToS3(thumbnailOutputStream, thumbnailFilename)
      QLog.spew("Uploaded!")
      
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
          }
            yield 
            {
              QLog.spew(s"Removing ${oldFilename.text} and ${oldThumbFilename.text}")
              s3client.deleteObject(AWS.bucket, oldFilename.text); 
              s3client.deleteObject(AWS.bucket, oldThumbFilename.text) 
            }

          result._1
        }
        case None => prop.cType.makePropValue(Some(elem), PhotosInternal.PhotoType)
      }
      
      sender ! PhotoInfo(qv)
      
      // Okay, start shutting down...
      originalImage.flush()
      self ! ImageComplete
    }
    
    case ImageComplete => {
      context.stop(self)
    }
  }
}

object PhotoUploadActor {
  def actorProps(ecology:Ecology, state:SpaceState):Props = Props(classOf[PhotoUploadActor], ecology, state) 
}
