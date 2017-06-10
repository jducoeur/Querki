package querki

import querki.globals._

package object aws {
  trait AWS extends EcologyInterface {
    /**
     * The URL for the root of Querki's bucket in S3.
     * 
     * Note that most content is actually under a Space's ID.
     */
    def S3BucketUrl:String
    
    /**
     * Given a Space and a file you want to stuff into S3 for that Space, get its
     * URL.
     * 
     * Prefer this over using S3BucketUrl yourself.
     */
    def fullS3Url(spaceId:OID, filename:String):String
  }
}
