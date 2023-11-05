package compiler

enum TokenType(val content: String, val tag: String):
  case KW(x: String)  extends TokenType(x, "keyword")
  case ID(x: String)  extends TokenType(x, "identifier")
  case NUM(x: String) extends TokenType(x, "number")
  case STR(x: String) extends TokenType(x, "string")
  case SP(x: String)  extends TokenType(x, "special")
  case Eof            extends TokenType("<eof>", "<eof>")

  override def toString(): String = s"$content"

object Tokenizer:
  case class Location(line: Int, column: Int):
    override def toString(): String = s"[$line:$column]"

  case class Token(l: Location, cont: TokenType):
    override def toString(): String = s"$cont"

  private enum TState:
    case None, Ident, Integral, Number, Str, Special
  import TState.*

  private val isSpecial = Set(
    '+', '-', '*', '/', '%', '^', '!', '=', '<', '>', '&', '|', '~', '(', ')',
    '[', ']', '{', '}', ',', ';', ':', '.', '\'', '"', '#'
  )

  inline def err(msg: String) = throw Exception(s"lex error: $msg")

  private def asState(c: Char): TState =
    if c.isLetter || c == '_' then Ident
    else if c.isDigit then Integral
    else if c == '"' then Str
    else if isSpecial(c) then Special
    else err(s"(${c.toInt})starter should produce non-empty state")

  private def process(
      state: TState,
      c: Char,
      cval: String
  ): (TState, String, String) = state match
    case None =>
      if c.isLetter || c == '_' then (Ident, cval + c, "")
      else if c.isDigit then (Integral, cval + c, "")
      else if c == '"' then (Str, cval + c, "")
      else if isSpecial(c) then (Special, cval + c, "")
      else (asState(c), s"$c", cval)
    case Ident =>
      if c.isLetterOrDigit || c == '_' then (Ident, cval + c, "")
      else (asState(c), s"$c", cval)
    case Integral =>
      if c.isDigit then (Integral, cval + c, "")
      else if c == '.' then (Number, cval + c, "")
      else (asState(c), s"$c", cval)
    case Number =>
      if c.isDigit then (Number, cval + c, "")
      else (asState(c), s"$c", cval)
    case Str =>
      assert(cval.nonEmpty)
      val last = cval.last
      if c == '"' && last != '\\' then (None, "", cval + c)
      else (Str, cval + c, "")
    case Special =>
      cval match
        case "." =>
          c match // concat or number
            case '.'            => (None, "", "..")
            case d if d.isDigit => (Number, cval + c, "")
            case _              => (asState(c), s"$c", cval)
        case "<" | ">" | "=" | "~" => // comparison
          if c == '=' then (None, "", cval + c)
          else (asState(c), s"$c", cval)
        case _ => (asState(c), s"$c", cval)

  val Keywords = Set[String](
    "and", "or", "not", "if", "then", "else", "elseif", "end", "while", "do",
    "repeat", "until", "for", "in", "break", "function", "local", "return",
    "true", "false", "nil"
  )

  private def mkTok(
      state: TState,
      curval: String,
      curloc: Location
  ): Token = state match
    case None => err("attempt to make token from empty state")
    case Ident =>
      Token(
        curloc,
        if Keywords.contains(curval)
        then TokenType.KW(curval)
        else TokenType.ID(curval)
      )
    case Integral | Number => Token(curloc, TokenType.NUM(curval))
    case Str               => Token(curloc, TokenType.STR(curval))
    case Special           => Token(curloc, TokenType.SP(curval))

  private def m_tokenize(
      s: List[Char],
      state: TState,
      curval: String,
      curloc: Location,
      tokens: List[Token]
  ): List[Token] =
    s match
      case Nil =>
        if curval.isEmpty then tokens
        else mkTok(state, curval, curloc) :: tokens
      case c :: rest =>
        val Location(line, column) = curloc
        if c.isWhitespace && state != Str then
          val nloc = c match
            case '\n' => Location(line + 1, 0)
            case '\t' => Location(line, column + 4 + curval.length)
            case _    => Location(line, column + 1 + curval.length)
          assert(
            state != Number || curval != ".",
            s"malformed numeric literal '$curval'"
          )
          val toks =
            if curval.isEmpty then tokens
            else mkTok(state, curval, curloc) :: tokens
          m_tokenize(rest, None, "", nloc, toks)
        else
          val (nState, newCur, newVal) = process(state, c, curval)
          val nTokens =
            if newVal.isEmpty then tokens
            else mkTok(state, newVal, curloc) :: tokens
          val nloc = curloc.copy(column = column + newVal.length)
          m_tokenize(rest, nState, newCur, nloc, nTokens)

  def tokenize(s: String) =
    (Token(Location(-1, -1), TokenType.Eof)
      :: m_tokenize(s.toList, None, "", Location(0, 0), Nil)).reverse
end Tokenizer