package querki.api

/**
 * The structure we send back from every API response. This is *mostly* just the uPickled
 * response to the call, but also includes some standard metadata. 
 * 
 * @author jducoeur
 */
case class ResponseWrapper(payload:String)
