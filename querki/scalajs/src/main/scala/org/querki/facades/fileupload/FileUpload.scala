package org.querki.facades.fileupload

import scala.scalajs.js
import org.querki.jquery.{JQueryEventObject, JQueryXHR}
import org.querki.jsext._

/**
 * Facade around the blueimp File Upload libraries. See
 * 
 *     https://github.com/blueimp/jQuery-File-Upload
 *     
 * for additional information.
 * 
 * This is currently a very partial facade. At some point, though, we may want to OSS
 * it and encourage others to help fill it in.
 */
trait FileUpload extends js.Object {
  def fileupload(options:FileUploadOptions):Any = js.native
}

trait FileUploadOptions extends js.Object

/**
 * Options available to fileupload().
 * 
 * TODO: flesh this out more! There are many, many options available in the library; see
 * 
 *     https://github.com/blueimp/jQuery-File-Upload/wiki/Options
 *     
 * for the full list.
 */
class FileUploadOptionBuilder(val dict:OptMap) extends JSOptionBuilder[FileUploadOptions, FileUploadOptionBuilder](new FileUploadOptionBuilder(_)) {
  
  /**
   * Disables the resize image functionality.
   * 
   * Default: true
   */
  def disableImageResize(v:Boolean) = jsOpt("disableImageResize", v)
  
  /**
   * The maximum height of resized images.
   * 
   * Default: 5000000
   */
  def imageMaxHeight(v:Int) = jsOpt("imageMaxHeight", v)
  
  /**
   * The maximum width of resized images.
   * 
   * Default: 5000000
   */
  def imageMaxWidth(v:Int) = jsOpt("imageMaxWidth", v)
  
  /**
   * The maximum allowed file size in bytes.
   * 
   * Default: undefined
   * Example: 10000000 // 10 MB
   * Note: This option has only an effect for browsers supporting the File API.
   */
  def maxFileSize(v:Int) = jsOpt("maxFileSize", v)
  
  /**
   * By default, XHR file uploads are sent as multipart/form-data.
   * The iframe transport is always using multipart/form-data.
   * If this option is set to false, the file content is streamed to the server url instead of 
   * sending a RFC 2388 multipart message for XMLHttpRequest file uploads.
   * Non-multipart uploads are also referred to as HTTP PUT file upload.
   * 
   * 
   * Note: Additional form data is ignored when the multipart option is set to false.
   * Non-multipart uploads (multipart: false) are broken in Safari 5.1 - see issue #700.
   * 
   * Default: true
   */
  def multipart(v:Boolean) = jsOpt("multipart", v)
  
  /**
   * A string containing the URL to which the request is sent.
   * 
   * If undefined or empty, it is set to the action property of the file upload form if available, or else the URL of the current page.
   */
  def url(v:String) = jsOpt("url", v)
  
  /////////////////////////////////////////
  //
  // Callbacks
  //
  
  def done(v:js.Function2[JQueryEventObject, FileUploadResults, Any]) = jsOpt("done", v)
  
  /**
   * Callback for upload progress events.
   */
  def progress(v:js.Function2[JQueryEventObject, FileUploadProgress, Any]) = jsOpt("progress", v)
  
  /**
   * Callback for uploads start, equivalent to the global ajaxStart event (but for file upload requests only).
   */
  def start(v:js.Function1[JQueryEventObject, Any]) = jsOpt("start", v)
  
  /**
   * Callback for the start of the fileupload processing queue.
   */
  def processstart(v:js.Function1[JQueryEventObject, Any]) = jsOpt("processstart", v)
}

object FileUploadOptions extends FileUploadOptionBuilder(noOpts)

trait FileUploadProgress extends js.Object {
  def loaded:Int = js.native
  def total:Int = js.native
}

/**
 * TODO: is this signature correct? The documentation of the result from done() is vague.
 */
trait FileUploadResults extends js.Object {
  def result:Any = js.native
  def textStatus:String = js.native
  def jqXHR:JQueryXHR = js.native
}
