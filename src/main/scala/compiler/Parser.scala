package compiler

import Tokenizer.Token
import Tokenizer.Location
import TokenType.*
// parse a limited subset of lua
// - no generic fors
// - no tables (only array-like)
//    - no metamethods
// - no repeat loops
// - no generic for loops
// - no multiple assignments
// - no varargs (yet)
// - no std lib (yet)
// - no bitwise operators
// lua grammar:
/*
chunk ::= {stat [`;`]} [laststat [`;`]]
block ::= chunk
stat ::=  Id `=` exp |
    functioncall |
    do block end |
    while exp do block end |
    if exp then block {elseif exp then block} [else block] end |
    for Id `=` exp `,` exp [`,` exp] do block end |
    local function Id funcbody |
    local Id [`=` Id]
laststat ::= return [exp] | break
var ::=  Id | prefixexp `[` exp `]` | prefixexp `.` Id
idlist ::= Id {`,` Id}
explist ::= {exp `,`} exp
exp ::=  nil | false | true | Number | String | `...` | function |
    prefixexp | tableconstructor | exp binop exp | unop exp
prefixexp ::= var | functioncall | `(` exp `)`
functioncall ::=  prefixexp args
args ::=  `(` [explist] `)` | tableconstructor | String
function ::= function funcbody
funcbody ::= `(` [parlist] `)` block end
parlist ::= idlist
tableconstructor ::= `{` [fieldlist] `}`
fieldlist ::= exp {fieldsep exp} [`;`]
field ::= `[` exp `]` `=` exp | Id `=` exp | exp
fieldsep ::= `,` | `;`
binop ::= `+` | `-` | `*` | `/` | `^` | `%` | `..` |
    `<` | `<=` | `>` | `>=` | `==` | `~=` |
    and | or
unop ::= `-` | not | `#`
 */

object Parser:
  import TreeNode.*

  type ParseResult = (Option[TreeNode], List[Token])
  type CertainParseResult = (TreeNode, List[Token])

  extension (pr: CertainParseResult)
    def toParseResult: ParseResult = (Some(pr._1), pr._2)

  inline def rmap(f: TreeNode => TreeNode)(res: ParseResult): ParseResult =
    res match
      case (Some(node), rest) => (Some(f(node)), rest)
      case other              => other
  inline def assert(cond: Boolean, msg: String) = if !cond then err(msg)
  inline def err(msg: String) = throw Exception(s"parse error: $msg")
  inline def peek(cur: List[Token], p: TokenType => Boolean, i: Int): Boolean =
    if cur.size > i then p(cur(i).cont) else false
  // assertion that a token satisfies a predicate
  inline def expect(
      cur: List[Token],
      p: TokenType => Boolean,
      msg: String,
      i: Int = 0
  ): Unit =
    if cur.size > i then
      if !peek(cur, p, i) then err(s"$msg: ${cur(i)}")
    else err(s"$msg: <eof>")

  // table constructor: {<explist> seperate by ','}
  def table(cur: List[Token], l : Location): CertainParseResult =
    val (fields, rest2) = fieldList(cur)
    expect(rest2, _ == SP("}"), s"expected '}' to close '{' from $l")
    (Arr(fields), rest2.tail)

  // function arguments : either (<exprlist>), "str", or {tab_cons}
  def farg(cur: List[Token]): (Option[List[TreeNode]], List[Token]) =
    cur match
      case Token(l, SP("(")) :: rest =>
        exprList(rest) match
          case (args, Token(_, SP(")")) :: rest2) => (Some(args), rest2)
          case _ => err(s"expected ')' to close '(' from $l")
      case Token(l, STR(name)) :: rest => (Some(List(LStr(name))), rest)
      case Token(l, SP("{")) :: rest =>
        table(rest, l) match
          case (arr, rest2) => (Some(List(arr)), rest2)
      case _ => (None, cur)

  // factor that starts with an id (either a function call or an id)
  def idExpr(name: String, cur: List[Token]): CertainParseResult =
    val (args, rest) = farg(cur)
    args match
      case Some(as) => (FunCall(name, as), rest)
      case None     => (Id(name), cur)

  // generic left associative op-seperated parsing
  private def tailGo(
      node: TreeNode,
      cur: List[Token],
      ops: Set[String],
      parser: List[Token] => ParseResult
  ): CertainParseResult =
    cur match
      case Token(_, op) :: rest if ops.contains(op.content) =>
        parser(rest) match
          case (Some(right), rest2) =>
            tailGo(BinOp(op.content, node, right), rest2, ops, parser)
          case other => (node, cur)
      case other => 
        (node, other)

  /**
   * helper to parse a list of patterns seperated by a set of seperators in a
   * left-associative manner
   *
   * @param parser
   *   parser for the pattern
   * @param seps
   *   set of seperators, usually operators
   * @param cur
   *   current token list
   * @return
   *   parse result
   */
  private def repSep(
      parser: List[Token] => ParseResult,
      seps: Set[String]
  )(
      cur: List[Token]
  ): ParseResult = parser(cur) match
    case (Some(lhs), rest) => tailGo(lhs, rest, seps, parser).toParseResult
    case other             => other

  // exp binop/unop established vis a vis pratt parser
  /*
  operator precedence:
  -  ^
  -  not  - (unary)
  -  *   /
  -  +   -
  -  ..
  -  <   >   <=  >=  ~=  ==
  -  and
  -  or
   */
  private def factor(cur: List[Token]): ParseResult =
    cur match
      case Nil                        => (None, Nil)
      case Token(_, ID(name)) :: rest => idExpr(name, rest).toParseResult
      case Token(l, SP("(")) :: rest =>
        val (exp, rest2) = expr(rest)
        expect(rest2, _ == SP(")"), s"expected ')' to close '(' from $l")
        (exp, rest2.tail)
      // unambiguous literals
      case Token(_, NUM(value)) :: rest  => (Some(LNum(value.toDouble)), rest)
      case Token(_, STR(value)) :: rest  => (Some(LStr(value)), rest)
      case Token(_, KW("nil")) :: rest   => (Some(LNil), rest)
      case Token(_, KW("true")) :: rest  => (Some(LBool(true)), rest)
      case Token(_, KW("false")) :: rest => (Some(LBool(false)), rest)
      case Token(l, SP("{")) :: rest     => table(rest, l).toParseResult
      case _                             => (None, cur) // soft fail
  // table indexing expression
  private def tabInd(cur: List[Token]): ParseResult =
    def go(toks: List[Token], node: TreeNode): ParseResult =
      toks match
        case Token(_, SP("[")) :: rest =>
          expr(rest) match
            case (Some(ind), Token(_, SP("]")) :: rest2) =>
              go(rest2, TInd(node, ind))
            case _ => err(s"expected `]` after `[`")
        case _ => (Some(node), toks)
    factor(cur) match
      case (Some(left), rem) => go(rem, left)
      case res               => res

  private def power(cur: List[Token]): ParseResult =
    tabInd(cur) match
      case (Some(left), Token(_, SP("^")) :: rest) =>
        rmap(BinOp("^", left, _))(unop(rest))
      case other => other

  private def unop(cur: List[Token]): ParseResult =
    cur match
      case Token(_, SP(op)) :: next if op == "-" || op == "#" =>
        rmap(UnOp(op, _))(unop(next))
      case _ => power(cur)
  // ops w/ same precedence as multiplication
  val mult   = repSep(unop, Set("*", "/", "%"))
  val add    = repSep(mult, Set("+", "-"))
  val concat = repSep(add, Set(".."))
  val comp   = repSep(concat, Set("<", "<=", ">", ">=", "~=", "=="))
  val and    = repSep(comp, Set("and"))
  val expr   = repSep(and, Set("or")) // lowest precedence - general expr

  // parse seperated list, expect that opening sym already consumed
  private def sepList(
      parser: List[Token] => ParseResult,
      seps: Set[String],
      term: String
  )(
      cur: List[Token],
      ret: List[TreeNode]
  ): (List[TreeNode], List[Token]) =
    parser(cur) match
      case (Some(node), r @ Token(_, SP(`term`)) :: _) =>
        ((node :: ret).reverse, r) // terminated
      case (Some(node), Token(_, SP(s)) :: rest) if seps(s) =>
        sepList(parser, seps, term)(rest, node :: ret) // new elem
      case other => (ret, other._2) // no more elems

  // should consume closing brace / paren, or error
  private inline def exprList(
      cur: List[Token],
      ret: List[TreeNode] = Nil
  ) = sepList(expr, Set(","), ")")(cur, ret)

  private inline def fieldList(
      cur: List[Token],
      ret: List[TreeNode] = Nil
  ) = sepList(expr, Set(",", ";"), "}")(cur, ret)

  def stmtList(cur: List[Token]): (List[TreeNode], List[Token]) =
    cur match
      case List(Token(_, Eof))       => (Nil, cur)
      case Token(_, SP(";")) :: rest => stmtList(rest)
      case _ =>
        statement(cur) match
          case (Some(node), rest) => // matched statement, keep matching
            val (nodes, rest2) = stmtList(rest)
            (node :: nodes, rest2)
          case (None, rest) => // failed to match, terminate
            (Nil, rest)

  def chunk(cur: List[Token]): CertainParseResult =
    val (stmts, rest) = stmtList(cur)
    lastStat(rest) match
      case (Some(node), rest2) => (Chunk(stmts :+ node), rest2)
      case (None, rest2)       => (Chunk(stmts), rest2)

  // last statement of a program
  private transparent inline def lastStat(cur: List[Token]) = cur match
    case Token(l, KW("return")) :: rest =>
      expr(rest) match
        case (Some(node), rest2) => (Some(Return(node)), rest2)
        case _                   => (Some(Return(LNil)), rest)
    case Token(_, KW("break")) :: rest => (Some(Break), rest)
    case _                             => (None, cur)

  private val idp = (cur: List[Token]) =>
    cur match
      case Token(_, ID(name)) :: rest => (Some(Id(name)), rest)
      case _                          => (None, cur)
  private def funDef(cur: List[Token]): ParseResult =
    cur match
      case Token(_, KW("local")) // should start w. [local function <name> (]
          :: Token(_, KW("function"))
          :: Token(l, ID(name))
          :: Token(pl, SP("(")) :: rest =>
        val (args, rest2) = sepList(idp, Set(","), ")")(rest, Nil)
        expect(rest2, _ == SP(")"), s"expected ')' to close '(' from $pl")
        val argList = args.map(_.asInstanceOf[Id].name)
        val (body, rem) = chunk(rest2.tail) match
          case (body, rem) =>
            expect(rem, _ == KW("end"), s"expected `end` after function at $l")
            (body, rem.tail)
        (Some(FunDef(name, argList, body)), rem)
      case _ => err(s"expected `local function <name>` at ${cur.head.l}}")

  private def varDef(cur: List[Token]): ParseResult =
    cur match
      case Token(_, KW("local")) // should start w. [local <name> = ]
          :: Token(_, ID(name))
          :: Token(_, SP("=")) :: rest =>
        expr(rest) match
          case (Some(value), rest2) => (Some(VarDef(name, value)), rest2)
          case (None, rest) => err(s"expected expression after `$name =`")
      case _ =>
        err(
          s"expected `local <name> =` at ${cur.head.l}, "
            + s"instead got `${cur.take(3).mkString(" ")}`"
        )

  private def whileLoop(cur: List[Token], l: Location): CertainParseResult = 
    // should start with <expr> do <block> end
    expr(cur) match
      case (Some(cond), Token(ld, KW("do")) :: rem) =>
        chunk(rem) match
          case (body, Token(le, KW("end")) :: rem2) => (While(cond, body), rem2)
          case _         => err(s"expected `end` after while body at $l")
      case (None, _) => err(s"expected expression after `while` at $l")
      case _         => err(s"expected `do` after `while` at $l")

  private inline def forBody(
      start: TreeNode,
      end: TreeNode,
      step: TreeNode,
      cur: List[Token]
  ): CertainParseResult = chunk(cur) match
    case (body, Token(_, KW("end")) :: rest) =>
      (For("i", start, end, step, body), rest)
    case _ => err(s"malformed for loop body at ${cur.head.l}")

  private def forLoop(cur: List[Token]): CertainParseResult = cur match
    case Token(l, KW("for"))
        :: Token(_, ID(name))
        :: Token(_, SP("=")) :: rest =>
      expr(rest) match
        case (Some(start), Token(_, SP(",")) :: rest2) =>
          expr(rest2) match
            case (Some(end), rest3) =>
              rest3 match
                case Token(_, SP(",")) :: rest4 =>
                  expr(rest4) match
                    case (Some(step), Token(_, KW("do")) :: rest5) =>
                      forBody(start, end, step, rest5)
                    case _ => err(s"bad step/ missing 'do' in loop at $l")
                case Token(_, KW("do")) :: rest4 =>
                  forBody(start, end, LNum(1), rest4)
                case _ =>
                  err(s"expected step or 'do' in for loop header after $l")
            case (None, _) =>
              err(s"error parsing ending value of `for` loop defined at $l")
            case _ => err(s"expected `do` after `,` at $l")
        case _ => err(s"error parsing inital value of `for` loop defined at $l")
    case _ => err(s"bad `for` preamble at ${cur.head.l}")

  // {`elseif` exp `then` block}
  private def elseifs(
      cur: List[Token],
      ret: List[(TreeNode, TreeNode)] = Nil
  ): (List[(TreeNode, TreeNode)], List[Token]) =
    cur match
      case Token(el, KW("elseif")) :: rest =>
        expr(rest) match
          case (Some(cond), Token(tl, KW("then")) :: rest2) =>
            val (body, rest3) = chunk(rest2)
            elseifs(rest3, (cond, body) :: ret)
          case (Some(_), rest2) =>
            err(s"expected `then` after `elseif` $el")
          case (None, _) =>
            err(s"expected condition after `elseif` $el")
      case _ => (ret, cur) // exit

  // full if-{elseif}-[else]-end block
  private def ifBlock(cur: List[Token]): ParseResult =
    expr(cur) match
      case (Some(cond), Token(_, KW("then")) :: rest) =>
        val (body, rest2) = chunk(rest)
        val (elifs, rest3) = elseifs(rest2)
        val (elseBlock, rem) = rest3 match
          case Token(_, KW("else")) :: rem =>
            chunk(rem) match
              case (node, Token(_, KW("end")) :: rest) =>
                (Some(node), rest)
              case (_, r) =>
                err(s"expected else body/end after ${r.head.l}")
          case Token(_, KW("end")) :: rem => (None, rem)
          case _ => err(s"expected `else` or `end` ifs and elseifs")
        (Some(If(cond, body, elifs, elseBlock)), rem)
      case (Some(_), rest) =>
        err(s"expected `then` after if after ${cur.head.l}")
      case (None, _) => err(s"expected expression after if after ${cur.head.l}")
  // stat ::=  Id `=` exp |
  //   functioncall |
  //   do block end |
  //   while exp do block end |
  //   if exp then block {elseif exp then block} [else block] end |
  //   for Id `=` exp `,` exp [`,` exp] do block end |
  //   local function Id funcbody |
  //   local Id [`=` Id]
  private def statement(cur: List[Token]): ParseResult = cur match
    case Token(_, KW("local")) :: rest =>
      rest match
        case Token(_, KW("function")) :: _ => funDef(cur)
        case _                             => varDef(cur)
    case Token(l, KW("while")) :: rest => whileLoop(rest, l).toParseResult
    case Token(_, KW("for")) :: rest   => forLoop(cur).toParseResult
    case Token(_, KW("if")) :: rest    => ifBlock(rest)
    case Token(_, ID(name)) :: rest =>
      // Attempt to parse a variable (lvalue) or function call
      // Reuse tabInd to handle chain of indexing: id[exp][exp]...
      tabInd(cur) match
        case (Some(lhs), Token(_, SP("=")) :: rest2) =>
          // It is an assignment
          expr(rest2) match
            case (Some(rhs), rest3) =>
              lhs match
                case Id(n)      => (Some(VarMut(n, rhs)), rest3)
                case TInd(t, i) => (Some(TableSet(t, i, rhs)), rest3)
                case _          => err(s"invalid assignment target ${lhs}")
            case _ => err("expected expression after `=`")
        case (Some(lhs), rest2) =>
          // Expression statement (function call)
          (Some(lhs), rest2)
        case _ => (None, cur)
    case _ => (None, cur)

  // just a chunk with added checks for EOF
  def program(cur: List[Token]): TreeNode =
    chunk(cur) match
      case (x, Token(_, Eof) :: Nil) => x
      case other =>
        err(s"expected <eof> after program, got [${other._2.mkString(", ")}]")
end Parser
