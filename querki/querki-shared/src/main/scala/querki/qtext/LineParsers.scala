package querki.qtext

import scala.util.parsing.input.{Position, Reader}


/**
 * Represents a line of markdown.
 * The prefix is the beginning of the line that indicates the line type,
 * the payload is the actual content after the prefix.
 */
sealed abstract class MarkdownLine(val prefix:String, val payload:String){
    /**
     * Constructs a MarkdownLine where the prefix is the empty String and the
     * payload is the whole line.
     */
    def this(c:String) = this ("", c)

    /**
     * Returns the full line as it was originally, i.e. prefix+payload.
     */
    def fullLine = prefix + payload
}

/**Represents lines of verbatim xml.
 * Actually this class is a little cheat, as it represents multiple lines.
 * But it is a token that is created when "parsing with a line scope", so it is not too bad.
 */
case class XmlChunk(content:String) extends MarkdownLine(content)
/** Represents the underline for a setext style header
 */
case class SetExtHeaderLine(content:String, headerLevel:Int) extends MarkdownLine(content)

/**
 * An atx style header line.
 * Trims hashes automatically and determines the header level from them.
 */
case class AtxHeaderLine(pre:String, pay:String) extends MarkdownLine(pre, pay) {
    /** removes all whitespace, nl and trailing hashes from the payload
     * "  foo ##  \n" => "foo"
     */
    def trimHashes() = {
        val s = payload.trim
        var idx = s.length - 1
        while (idx >= 0 && s.charAt(idx) == '#') idx -= 1
        s.substring(0,idx+1).trim
    }

    def headerLevel = prefix.length
}
/** A line consisting only of whitespace.
 */
case class EmptyLine(content:String) extends MarkdownLine(content)
/** A horizontal ruler line.
 */
case class RulerLine(content:String) extends MarkdownLine(content)
/** A line indicating a block quote (starts with "> ")
 */
case class BlockQuoteLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** A line indicating the start of an unordered list item (starts with "   *  ")
 */
case class UItemStartLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** A line indicating the start of an ordered list item (starts with "   [NUMBER].  ")
 */
case class OItemStartLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** A line indicating the start of an definition list item (starts with "   : [TITLE] :  ")
 */
case class DItemStartLine(pre:String, pay:String, title:String) extends MarkdownLine(pre, pay)
/** A line in verbatim code or the continuation of a list item
 */
case class CodeLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** Starting line of a fenced code block: three backticks followed by an optional
 * language token
 */
case class ExtendedFencedCode(pre:String, pay:String) extends MarkdownLine(pre, pay) {
    def languageFormat = pay.trim()
}
/** Ending line of a fenced code block: three backticks followed by optional whitespace 
 */
case class FencedCode(pre:String) extends MarkdownLine(pre)
/** The beginning of a div for declaring a style class.
 */
case class ClassDivStartLine(content:String, className:String) extends MarkdownLine(content)
case class ClassDivEnd(content:String) extends MarkdownLine(content)
/** Any other line.
 */
case class OtherLine(content:String) extends MarkdownLine(content)


/** Definition of a link or url that can be referenced by id.
 */
case class LinkDefinition(id:String, url:String, title:Option[String])

/** Stub class that is an intermediate result when parsing link definitions.
 */
case class LinkDefinitionStart(id:String, url:String) {
    def toLinkDefinition(title:Option[String]) = new LinkDefinition(id, url, title)
}

/**
 * A flag that changes the state of the document.
 */
case class ParseFlag(name:String, v:String) extends MarkdownLine(name, v)

/**
 * This class allows us to reference a map with link definitions resulting from the line parsing during block parsing.
 * It extends a Reader for MarkdownLines and allows us to add the said map to the parsing context.
 * This is basically a modification of the parser monad's state. 
 */
case class MarkdownLineReader private (val linesIn:Seq[MarkdownLine],
                                       val lookup:Map[String, LinkDefinition],
                                       val lineCountIn:Int,
                                       val flagsIn:Map[String, Int])
        extends Reader[MarkdownLine] {
    /** Not existing line that signals EOF.
     * This object cannot be referenced by any other code so it will fail all line parsers. 
     */
    private object EofLine extends MarkdownLine("\nEOF\n")
    
    def handleFlag(curFlags:Map[String, Int], flag:ParseFlag):Map[String, Int] = {
      // For now, we only deal with simple binary flags:
      flag.v match {
        case "+" => {
          if (curFlags.contains(flag.name))
            curFlags + (flag.name -> (curFlags(flag.name) + 1))
          else
            curFlags + (flag.name -> 1)
        }
        case "-" => {
          val curLevelOpt = curFlags.get(flag.name)
          curLevelOpt match {
            case Some(n) if (n > 1) => curFlags + (flag.name -> (n - 1))
            case Some(n) => curFlags - flag.name
            case None => curFlags
          }
        }
        case _ => curFlags
      }
    }
    
    def handleFlagsRec(curLines:Seq[MarkdownLine], curLineCount:Int, curFlags:Map[String, Int]):(Seq[MarkdownLine], Int, Map[String, Int]) = {
      if (curLines.isEmpty)
        (curLines, curLineCount, curFlags)
      else curLines.head match {
        case flag:ParseFlag => handleFlagsRec(curLines.tail, curLineCount + 1, handleFlag(curFlags, flag))
        case _ => (curLines, curLineCount, curFlags)
      }
    }
    
    // Strip off any leading flag lines, and incorporate them into current processing:
    val (lines, lineCount, flags) = handleFlagsRec(linesIn, lineCountIn, flagsIn)


    def this(ls:Seq[MarkdownLine], lu:Map[String, LinkDefinition]) = this(ls, lu, 1, Map.empty)
    def this(ls:Seq[MarkdownLine]) = this (ls, Map())
    def first = if (lines.isEmpty) EofLine else lines.head
    def rest  = if (lines.isEmpty) this else new MarkdownLineReader(lines.tail, lookup, lineCount + 1, flags)
    def atEnd = lines.isEmpty
    def pos   = new Position {
        def line   = lineCount
        def column = 1
        protected def lineContents = first.fullLine
    }
}

/**
 * Parses single lines into tokens.
 * Markdown lines are differentiated by their beginning.
 * These lines are then organized in blocks by the BlockParsers.
 */
trait LineParsers extends InlineParsers {

    /////////////////////////////////
    // Link definition pre-parsing //
    /////////////////////////////////

    /** The Start of a link definition: the id in square brackets, optionally indented by three spaces
     */
    def linkDefinitionId:Parser[String] =
        """ {0,3}\[""".r ~> markdownText(Set(']'), true) <~ ("]:" ~ ows) ^^ {_.trim.toLowerCase}
    /** The link url in a link definition.
     */
    def linkDefinitionUrl:Parser[String] =
        (elem('<') ~> markdownText(Set('>'), true) <~ '>' ^^ {_.mkString.trim}) |
        (markdownText(Set(' ','\t'), true) ^^ {_.mkString})
    /** The title in a link definition.
     */
    def linkDefinitionTitle:Parser[String] =
        ows ~> ("""\"[^\n]*["]""".r |
                """\'[^\n]*\'""".r  |
                """\([^\n]*\)""".r) <~ ows ^^ { s => s.substring(1,s.length-1) }

    /** A link definition that later gets stripped from the output.
     * Either a link definition on one line or the first line of a two line link definition.
     */
    def linkDefinitionStart:Parser[(LinkDefinitionStart, Option[String])] =
        linkDefinitionId ~ linkDefinitionUrl ~ opt(linkDefinitionTitle) ^^ {case i ~ u ~ t => (new LinkDefinitionStart(i, u), t)}


    //////////////////////////////////////////
    // Lines for XML Block tokenizing       //
    //////////////////////////////////////////

    /** A line that starts an xml block: an opening xml element fragment.
     */
    def xmlBlockStartLine:Parser[String] = guard('<' ~ xmlName) ~> rest
    /** A line that ends an xml block: a line starting with an xml end tag
     */
    def xmlBlockEndLine:Parser[String] = guard(xmlEndTag) ~> rest
    /** A line not starting with an xml end tag
     */
    def notXmlBlockEndLine:Parser[String] = not(xmlEndTag) ~> rest
    
    def pureXmlLine:Parser[String] = opt(ws) ~ rep1sep(xmlTag | xmlEndTag, opt(ws)) ^^ { case ws ~ xml => ws.getOrElse("") + xml.mkString }
    
    
    /////////////////
    // Parse flags //
    /////////////////
    
    def parseFlag:Parser[ParseFlag] = "!" ~> """\+|\-""".r ~ """\w+""".r ^^ {
      case ind ~ name => ParseFlag(name, ind)
    }


    //////////////////////////////
    // Markdown line tokenizing //
    //////////////////////////////

    /** Parses the line under a setext style level 1 header: =====
     */
    val setextHeader1:Parser[SetExtHeaderLine] = """=+([ \t]*)$""".r ^^ {new SetExtHeaderLine(_, 1)}

    /** Parses the line under a setext style level 2 header: -----
     */
    val setextHeader2:Parser[SetExtHeaderLine] = """((\-)+)([ \t]*)$""".r ^^ {new SetExtHeaderLine(_, 2)}

    /** Parses headers of the form: ### header ###
     */
    val atxHeader:Parser[AtxHeaderLine] = """#+""".r ~ rest ^^ {
        case prefix ~ payload => new AtxHeaderLine(prefix, payload)
    }

    /** Parses a horizontal rule.
     */
    val ruler:Parser[MarkdownLine] = """ {0,3}(((-[ \t]*){3,})|((\*[ \t]*){3,}))$""".r ^^ { new RulerLine(_) }

    /** Matches a line starting with up to three spaces, a '>' and an optional whitespace.
     * (i.e.: the start or continuation of a block quote.)
     */
    val blockquoteLine:Parser[BlockQuoteLine] = """ {0,3}\>( )?""".r ~ rest ^^ {
        case prefix ~ payload => new BlockQuoteLine(prefix,payload)
    }


    /** A line that starts an unordered list item.
     * Matches a line starting with up to three spaces followed by an asterisk, a space, and any whitespace.
     */
    val uItemStartLine:Parser[UItemStartLine] = (""" {0,3}[\*\+-] [\t\v ]*""".r) ~ rest ^^ {
        case prefix ~ payload => new UItemStartLine(prefix, payload)
    }


    /** A line that starts an ordered list item.
     * Matches a line starting with up to three spaces followed by a number, a dot and a space, and any whitespace
     */
    val oItemStartLine:Parser[OItemStartLine] = (""" {0,3}[0-9]+\. [\t\v ]*""".r) ~ rest ^^ {
        case prefix ~ payload => new OItemStartLine(prefix, payload)
    }

    /** A line that starts a definition list item.
     * Matches a line starting with up to three spaces followed by a colon, a space, a title, a space, a colon,
     * and any whitespace.
     */
    val dItemStartLine:Parser[DItemStartLine] = (""" {0,3}: """.r) ~ ("""(?:(?! :).)*""".r) ~ (""" : [\t\v ]*""".r) ~ rest ^^ {
        case start ~ title ~ end ~ payload => {
          val prefix = start + title + end
          new DItemStartLine(prefix, payload, title.trim)
        }
    }

    /** Accepts an empty line. (A line that consists only of optional whitespace or the empty string.)
     */
    val emptyLine:Parser[MarkdownLine] = """([ \t]*)$""".r ^^ {new EmptyLine(_)}

    /** Matches a code example line: any line starting with four spaces or a tab.
     */
    val codeLine:Parser[CodeLine] = ("    " | "\t") ~ rest ^^ {
        case prefix ~ payload => new CodeLine(prefix, payload)
    }
        
    /**
     * A fenced code line. Can be the start or the end of a fenced code block 
     */
    val fencedCodeLine:Parser[FencedCode] = """ {0,3}\`{3,}[\t\v ]*""".r ^^ {
        case prefix => new FencedCode(prefix) 
    }
        
    /** Matches the start of a fenced code block with additional language token: 
     * up to three spaces, three or more backticks, whitespace, an optional
     * language token, optional whitespace 
     */
    val extendedFencedCodeLine:Parser[ExtendedFencedCode] = fencedCodeLine ~ """\w+[\t\v ]*""".r ^^ {
        case prefix ~ languageToken => new ExtendedFencedCode(prefix.fullLine, languageToken) 
    }  
    
    /**
     * Matches the beginning of a style-class marker. Might wind up as a div or span, depending on whether
     * there is anything else on the line.
     */
    val classDivStart:Parser[ClassDivStartLine] = """ {0,3}\{\{ *""".r ~ rep1sep("""[\w\-]+""".r, " +".r) ~ """ *:""".r ^^ {
      case intro ~ classNameList ~ colon => {
        val classNames = classNameList.mkString(" ")
        val prefix = intro + classNames + colon
        new ClassDivStartLine(prefix, classNames)
      }
    } 
    val classDivEnd:Parser[ClassDivEnd] = """ {0,3}\}\}""".r ^^ { 
      case content => new ClassDivEnd(content) 
    }

    /** Matches any line. Only called when all other line parsers have failed.
     * Makes sure line tokenizing does not fail and we do not loose any lines on the way.
     */
    val otherLine:Parser[OtherLine] = rest ^^ {new OtherLine(_)}

    ///////////////////////////////////////////////////////////////
    // combined parsers for faster tokenizing based on lookahead //
    ///////////////////////////////////////////////////////////////
    /** First tries for a setext header level 2, then for a ruler.
     */
    val setext2OrRuler:Parser[MarkdownLine] = setextHeader2 | ruler
    /** First tries for a ruler, then for an unordered list item start.
     */
    val rulerOrUItem:Parser[MarkdownLine] = ruler | uItemStartLine
    /** First tries if the line is empty, if not tries for a code line.
     */
    val emptyOrCode:Parser[MarkdownLine] = emptyLine | codeLine
    
    /** Parses one of the fenced code lines
     */
    val fencedCodeStartOrEnd:Parser[MarkdownLine] = extendedFencedCodeLine | fencedCodeLine  
}

