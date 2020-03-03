package wuji

import scala.collection.mutable.ListBuffer

case class Parser(text:String) {
    private val input = text.toIterator

    private def streamFromIterator[A](iterator : Iterator[A]) : Stream[A] = {
        if (iterator.hasNext) {
            return iterator.next() #:: streamFromIterator(iterator)
        } else {
            return Stream.empty
        }
    }

    private val tokens = scala.collection.mutable.Queue[SObject]()
    private var location = 0
    private var start = location

    private case class left(location:SLocation) extends SObject {
        val names = List[String]()
    }
    private case class right(location:SLocation) extends SObject {
        val names = List[String]()
    }
    private case class hash(location:SLocation, value:String) extends SObject {
        val names = List[String]()
    }

    private trait processor {
        def next(c:Char)
    }

    private var processor:processor = whitespace

    private val whitespace_chars = Set('之', '乎', '者', '也', '又', '再', '还', '先', '兮', '为', '至')
    private case object whitespace extends processor {
        def next(c:Char) = {
            start = location
            c match {
                case '#' => {
                    val b = new StringBuilder()
                    processor = hash_processor(b)
                    start = location
                }
                case '"' | '“' => {
                    val b = new StringBuilder()
                    processor = string_processor(b)
                    start = location
                }
                case ';' | '；' => {
                    processor = comment
                }
                case '(' | '（' | '[' | '【'=> {
                    tokens.enqueue(left(SLocation(start, location)))
                    processor = whitespace
                }
                case ')' | '）' | ']' | '】'=> {
                    tokens.enqueue(right(SLocation(start, location)))
                    processor = whitespace
                }
                case '-' => {
                    val b = new StringBuilder()
                    b.append(c)
                    processor = long_processor(b)
                    start = location
                }
                case s if s.isDigit => {
                    val b = new StringBuilder()
                    b.append(c)
                    processor = long_processor(b)
                    start = location
                }
                case '，' | '。' | '！'=> {
                    processor = whitespace
                }
                case s if whitespace_chars contains s => {
                    processor = whitespace
                }
                case s if s.isWhitespace => {
                    processor = whitespace
                }
                case s if s.isLetter => {
                    val b = new StringBuilder()
                    b.append(c)
                    processor = symbol_processor(b)
                    start = location
                }
                case _ => throw ParseException(SLocation(start, location), "character not allowd")
            }
        }
    }

    private case object comment extends processor {
        def next(c:Char) = {
            if (c == '\n') processor = whitespace
        }
    }

    private case class hash_processor(b:StringBuilder) extends processor {
        def next(c:Char) = {
            c match {
                case '*' => {
                    b.append(c)
                }
                case s if s.isLetterOrDigit => {
                    b.append(c)
                }
                case _ => {
                    tokens.enqueue(hash((SLocation(start, location)), b.mkString))
                    whitespace.next(c)
                }
            }
        }
    }

    private case class string_processor(b:StringBuilder) extends processor {
        var convert = false
        def next(c:Char) = {
            if(!convert) {
                if(c == '\\') {
                    convert = true
                } else if (
                    c == '”' ||
                    c == '"'
                ) {
                    tokens.enqueue(SString(SLocation(start, location), b.mkString))
                    processor = whitespace
                } else {
                    b.append(c)
                }
            } else {
                b.append(c)
                convert = false
            }
        }
    }

    private case class long_processor(b:StringBuilder) extends processor {
        def next(c:Char) = {
            if(c.isDigit) {
                b.append(c)
            } else if(c == '.') {
                b.append(c)
                processor = double_processor(b)
            } else {
                try {
                    val value = b.mkString.toLong
                    tokens.enqueue(SInt(SLocation(start, location), value))
                } catch {
                    case e:Throwable => throw ParseException(SLocation(start, location), "error in parsing SInt")
                }
                whitespace.next(c)
            }
        }
    }

    private case class double_processor(b:StringBuilder) extends processor {
        def next(c:Char) = {
            if(c.isDigit) {
                b.append(c)
            } else if(c == '.') {
                throw ParseException(SLocation(start, location), "error in parsing SFloat")
            } else {
                val s = b.mkString
                try {
                    tokens.enqueue(SFloat(SLocation(start, location), s.toDouble))
                } catch {
                    case _:Throwable => throw ParseException(SLocation(start, location), "error in parsing SFloat")
                }
                whitespace.next(c)
            }
        }
    }

    private case class symbol_processor(b:StringBuilder) extends processor {
        def next(c:Char) = {
            c match {
                case s if whitespace_chars contains s => {
                    tokens.enqueue(SSymbol(SLocation(start, location), b.mkString))
                    whitespace.next(c)
                }
                case s if s.isLetter => {
                    b.append(c)
                }
                case '_' | '-' | '.' => {
                    b.append(c)
                }
                case c:Char => {
                    tokens.enqueue(SSymbol(SLocation(start, location), b.mkString))
                    whitespace.next(c)
                }
            }
        }
    }

    private def next_token() = {
        while(tokens.isEmpty) {
            if(input.hasNext) {
                val c = input.next()
                location = location + 1
                processor.next(c)
            } else {
                processor.next('\n')
                tokens.enqueue(SExit(SLocation(location, location)))
            }
        }

        tokens.dequeue()
    }

    def next():SObject = {
        next_token() match {
            case left(location) => nextList(start)
            case h:hash => next_hash(h)
            case e:SObject => e
        }
    }

    private def next_hash(h:hash):SObject = {
        h.value match {
            case "true" => SBoolean(h.location, true)
            case "false" => SBoolean(h.location, false)
            case "exit" => SExit(h.location)
            case _ => SExit(h.location)
        }
    }

    private def nextList(start:Int):SObject = {
        val ls = ListBuffer[SObject]()
        var loc = 0

        while(loc == 0) {
            val token = next_token()
            token match {
                case right(location) => {
                    loc = location.end
                }
                case left(location) => ls.append(nextList(start))
                case SExit(location) => throw ParseException(location, "unexpected exit or end of file")
                case h:hash => ls.append(next_hash(h))
                case e:SObject => ls.append(e)
            }
        }

        SList(ls.toList)
    }
}