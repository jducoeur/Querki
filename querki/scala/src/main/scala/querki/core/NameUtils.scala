package querki.core

trait NameUtils {
  def toInternal(str:String) = str.replaceAll(" ", "-")
  def toDisplay(str:String) = str.replaceAll("-", " ")
  // Note that this deliberately allows mixed-case, so that we can preserve Name case through
  // the URL for Tags. (Since the desired case *only* exists in the URL.)
  def toUrl = toInternal _
  
  def canonicalize(str:String):String = toInternal(str).toLowerCase  
  
  def compareNames(left:String, right:String):Boolean = { left < right } 
      
  def makeLegal(str:String):String = str.filter(c => c.isLetterOrDigit || c == ' ' || c == '-')
    
  def equalNames(str1:String, str2:String):Boolean = {
    canonicalize(str1).equals(canonicalize(str2))
  }
}
// So that these functions can be used without mixing in NameUtils:
object NameUtils extends NameUtils
