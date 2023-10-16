  package compiler

  enum TokenType(val content: String):
    case Word(x: String) extends TokenType(x)
    case  Ident(x: String) extends TokenType(x)
    case Num(x: String) extends TokenType(x)
    case Str(x: String) extends TokenType(x)
    case Special(x: String) extends TokenType(x)
    case Comment(x: String) extends TokenType(x)
    case Eof extends TokenType("")

  object Tokenizer:
    case class Location(line: Int, column: Int) derives CanEqual
    

    case class Token(l: Location, tokType: TokenType)

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
      else if c == '.' then Number
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
        else if c == '.' then (Number, cval + c, "")
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
        else
          assert(cval != ".", s"malformed numeric literal '$cval'")
          (asState(c), s"$c", cval)
      case Str =>
        assert(cval.nonEmpty)
        val last = cval.last
        if c == '"' && last != '\\' then (None, "", cval + c)
        else (Str, cval + c, "")
      case Special =>
        if cval == "/" && c == '*' then (None, "", "/*")
        else if cval == "/" && c == '/' then (None, "", "//")
        else if cval == "*" && c == '/' then (None, "", "*/")
        else (asState(c), s"$c", cval)

    val Keywords = Set[String](
      "and", "or", "not",
      "if", "then", "else", "elseif", "end",
      "while", "do", "repeat", "until", "for", "in", "break",
      "function", "local", "return"
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
            then TokenType.Word(curval)
            else TokenType.Ident(curval)
        )
      case Integral | Number => Token(curloc, TokenType.Num(curval))
      case Str               => Token(curloc, TokenType.Str(curval))
      case Special           => Token(curloc, TokenType.Special(curval))

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
          println(s"$state [$line, $column] $c (${c.toInt}) ($curval)")
          if c.isWhitespace && state != Str then
            val nloc =  c match
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
      m_tokenize(s.toList, None, "", Location(0, 0), Nil).reverse
  end Tokenizer
