package querki.aws

import querki.ecology._
import querki.globals._

object MOIDs extends EcotIds(70) {
  
}

class AWSEcot(e:Ecology) extends QuerkiEcot(e) with AWS {
  
  /**
   * The root URL of Querki's bucket in S3.
   */
  lazy val S3BucketUrl = Config.getString("querki.aws.bucketUrl")

  def fullS3Url(spaceId:OID, filename:String):String = {
    s"$S3BucketUrl/${spaceId.toString}/filename"
  }
}
