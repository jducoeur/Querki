package querki.util

import org.querki.jquery._

/**
 * @author jducoeur
 */
object InputUtils {
  /**
   * This is a filter you can apply to RxText, to only allow in characters legal for names. Note
   * that this is deliberately set up for curried use.
   */
  def nameFilter(allowNonAlphaNumeric:Boolean, allowLeadingSpace:Boolean)(evt:JQueryEventObject, current:String):Boolean = {
    // TODO: this is quite crude, and doesn't allow Unicode through. We should do better.
    // See if there is a library to really do this sort of keyboard filtering well.
    val key = evt.which
    val c = key.toChar
    
    // Work around the idiotic Chrome mobile misfeature -- we simply give up for now:
    (key == 0 || key == 229) ||
    (c >= 'A' && c <= 'Z') || 
    // Numeric keypad
    (key >= 96 && key <= 105) ||
    // Backspace and Tab and Enter
    key == 8 || key == 9 || key == 13 ||
    // Home and End
    key == 35 || key == 36 ||
    // left, right, up and down arrows
    (key >= 37 && key <= 40) ||
    // Del and Ins
    key == 46 || key == 45 ||
    (!(evt.shiftKey.get) && 
      (c >= '0' && c <= '9') || 
      (allowNonAlphaNumeric &&
        (key == 189 || // dash 
        // Only allow space if there is already text *or* it's explicitly allowed.
        ((allowLeadingSpace || current.length > 0) && (c == ' ')))))
  }
  
  def spaceNameFilter(evt:JQueryEventObject, current:String):Boolean = nameFilter(true, false)(evt, current)
}