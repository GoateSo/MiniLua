package compiler

import Tokenizer.Token
import Tokenizer.Location
import TokenType.*
// TODO: include table indexing expressions
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
enum TreeNode:
  case BinOp(op: String, left: TreeNode, right: TreeNode)
  case UnOp(op: String, right: TreeNode)
  case TInd(tab: TreeNode, ind: TreeNode)
  case VarDef(name: String, value: TreeNode)
  case VarMut(name: String, value: TreeNode)
  case FunDef(name: String, args: List[String], body: TreeNode)
  case FunCall(name: String, args: List[TreeNode])
  case While(cond: TreeNode, body: TreeNode)
  case Break
  case For(
      name: String,
      start: TreeNode,
      end: TreeNode,
      step: Option[TreeNode],
      body: TreeNode
  )
  case If(
      cond: TreeNode,
      body: TreeNode,
      elifs: List[(TreeNode, TreeNode)], // elseif blocks (possibly empty)
      elseBody: Option[TreeNode]         // else block
  )
  case Chunk(stmts: List[TreeNode])
  case LNum(value: Double)
  case LBool(value: Boolean)
  case LStr(value: String)
  case LNil
  case Id(name: String)
  case Arr(fields: List[TreeNode])
  case Return(expr: TreeNode)

object Parser:
  import TreeNode.*

  type ParseResult = (Option[TreeNode], List[Token])
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
      msg: String = "unexpected token",
      i: Int = 0
  ): Unit =
    if cur.size > i then
      if !peek(cur, p, i) then err(s"$msg: ${cur(i)}")
    else err(s"$msg: <eof>")

  // table constructor
  def table(cur: List[Token]): ParseResult =
    cur match
      case Token(l, SP("{")) :: rest =>
        val (fields, rest2) = fieldList(rest)
        expect(rest2, _ == SP("}"), s"expected '}' to close '{' from $l")
        (Some(Arr(fields)), rest2.tail)
      case _ => (None, cur)

  // funcation arguments
  def farg(cur: List[Token]): (Option[List[TreeNode]], List[Token]) =
    cur match
      case Token(l, SP("(")) :: rest =>
        exprList(rest) match
          case (args, Token(_, SP(")")) :: rest2) => (Some(args), rest2)
          case _ => err(s"expected ')' to close '(' from $l")
      case Token(l, STR(name)) :: rest => (Some(List(LStr(name))), rest)
      case Token(l, SP("{")) :: _ =>
        table(cur) match
          case (Some(Arr(fields)), rest) => (Some(fields), rest)
          case _ => err(s"expected table after `{` at $l")
      case _ => (None, cur)

  // factor that starts with an id (either a function call or an id)
  def idExpr(cur: List[Token]): ParseResult =
    cur match
      case Token(_, ID(name)) :: rest =>
        val (args, rest2) = farg(rest)
        args match
          case Some(as) => (Some(FunCall(name, as)), rest2)
          case None     => (Some(Id(name)), rest)
      case _ => (None, cur)

  private def tailGo(
      node: TreeNode,
      cur: List[Token],
      ops: Set[String],
      parser: List[Token] => ParseResult
  ): (Option[TreeNode], List[Token]) =
    cur match
      case Token(_, op) :: rest if ops.contains(op.content) =>
        parser(rest) match
          case (Some(right), rest2) =>
            tailGo(BinOp(op.content, node, right), rest2, ops, parser)
          case other => (Some(node), other._2)
      case other => (Some(node), other)

  private def repSep(
      parser: List[Token] => ParseResult,
      seps: Set[String]
  )(
      cur: List[Token]
  ): ParseResult = parser(cur) match
    case (Some(lhs), rest) => tailGo(lhs, rest, seps, parser)
    case other             => other

  def factor(cur: List[Token]): ParseResult =
    cur match
      case Nil                        => (None, Nil)
      case Token(_, ID(name)) :: rest => idExpr(cur)
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
      case Token(_, SP("{")) :: rest     => table(cur)
      case _                             => (None, cur) // soft fail

  def tabInd(cur: List[Token]): ParseResult =
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

  def power(cur: List[Token]): ParseResult =
    tabInd(cur) match
      case (Some(left), Token(_, SP("^")) :: rest) =>
        rmap(BinOp("^", left, _))(power(rest))
      case other => other

  def unop(cur: List[Token]): ParseResult =
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
  def sepList(
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
  def exprList(
      cur: List[Token],
      ret: List[TreeNode] = Nil
  ): (List[TreeNode], List[Token]) =
    sepList(expr, Set(","), ")")(cur, ret)

  def fieldList(
      cur: List[Token],
      ret: List[TreeNode] = Nil
  ): (List[TreeNode], List[Token]) =
    sepList(expr, Set(",", ";"), "}")(cur, ret)

  def stmtList(cur: List[Token]): (List[TreeNode], List[Token]) =
    cur match
      case List(Token(_, Eof)) =>
        (Nil, cur)
      case Token(_, SP(";")) :: rest =>
        stmtList(rest)
      case _ =>
        statement(cur) match
          case (Some(node), rest) =>
            val (nodes, rest2) = stmtList(rest)
            (node :: nodes, rest2)
          case (None, rest) =>
            (Nil, rest)

  def chunk(cur: List[Token]): ParseResult =
    val (stmts, rest) = stmtList(cur)
    println(
      s"${summon[sourcecode.Line]}: stmts: $stmts | rest: $rest"
    )
    lastStat(rest) match
      case (Some(node), rest2) => (Some(Chunk(stmts :+ node)), rest2)
      case (None, rest2)       => (Some(Chunk(stmts)), rest2)

  def lastStat(cur: List[Token]): ParseResult = cur match
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
    // expect(cur, _ == KW("local"), "`local` not in fundef!")
    // expect(cur, _ == KW("function"), "`function` expected in fundef", 1)
    // expect(cur, _.isInstanceOf[ID], "name expected in fundef", 2)
    // expect(cur, _ == SP("("), "`(` expected in fundef", 3)
    cur match
      case Token(_, KW("local"))
          :: Token(_, KW("function"))
          :: Token(l, ID(name))
          :: Token(pl, SP("(")) :: rest =>
        val (args, rest2) = sepList(idp, Set(","), ")")(rest, Nil)
        expect(rest2, _ == SP(")"), s"expected ')' to close '(' from $pl")
        val argList = args.map(_.asInstanceOf[Id].name)
        val (body, rem) = chunk(rest2.tail) match
          case (Some(body), rem) =>
            expect(rem, _ == KW("end"), s"expected `end` after function at $l")
            (body, rem.tail)
          case (None, _) => err(s"expected body for function `$name` ($l)")
        (Some(FunDef(name, argList, body)), rem)
      case _ => err(s"expected `local function <name>` at ${cur.head.l}}")

  private def varDef(cur: List[Token]): ParseResult =
    cur match
      case Token(_, KW("local"))
          :: Token(_, ID(name))
          :: Token(_, SP("=")) :: rest =>
        expr(rest) match
          case (Some(value), rest2) => (Some(VarDef(name, value)), rest2)
          case (None, _) => err(s"expected expression after `$name =`")
      case _ =>
        err(
          s"expected `local <name> =` at ${cur.head.l}, "
            + s"instead got `${cur.take(3).mkString(" ")}`"
        )

  private def varMut(cur: List[Token]): ParseResult = cur match
    case Token(_, ID(name)) :: Token(_, SP("=")) :: rest =>
      expr(rest) match
        case (Some(value), rest2) => (Some(VarMut(name, value)), rest2)
        case (None, _)            => err(s"expected expression after `$name =`")
    case _ => err(s"expected `<name> =` at ${cur.head.l}")

  private def whileLoop(cur: List[Token]): ParseResult = cur match
    case Token(l, KW("while")) :: rest =>
      expr(rest) match
        case (Some(cond), Token(ld, KW("do")) :: rem) =>
          chunk(rem) match
            case (Some(body), Token(le, KW("end")) :: rem2) =>
              (Some(While(cond, body)), rem2)
            case (None, _) => err(s"expected body after `do` at ${rem.head.l}")
            case _         => err(s"expected `end` after while body at $l")
        case (None, _) => err(s"expected expression after `while` at $l")
        case _         => err(s"expected `do` after `while` at $l")
    case _ =>
      err(s"expected `while` at ${cur.head.l}")

  private def forLoop(cur: List[Token]): ParseResult = cur match
    case Token(l, KW("for"))
        :: Token(_, ID(name))
        :: Token(_, SP("=")) :: rest =>
      expr(rest) match
        case (Some(start), Token(_, SP(",")) :: rest2) =>
          expr(rest2) match
            case (Some(end), Token(_, KW("do")) :: rest3) =>
              chunk(rest3) match
                case (Some(body), Token(_, KW("end")) :: rest4) =>
                  (Some(For("i", start, end, None, body)), rest4)
                case (None, _) =>
                  err(s"expected body for loop defined at $l")
                case _ => err(s"expected `end` after `for` body after $l")
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
            chunk(rest2) match
              case (Some(body), rest3) =>
                elseifs(rest3, (cond, body) :: ret)
              case (None, _) =>
                err(s"expected body after `then` $tl")
          case (Some(_), rest2) =>
            err(s"expected `then` after `elseif` $el")
          case (None, _) =>
            err(s"expected condition after `elseif` $el")
      case _ => (ret, cur) // exit

  private def ifBlock(cur: List[Token]): ParseResult =
    expr(cur) match
      case (Some(cond), Token(_, KW("then")) :: rest) =>
        chunk(rest) match
          case (Some(body), rest2) =>
            val (elifs, rest3) = elseifs(rest2)
            val (elseBlock, rem) = rest3 match
              case Token(_, KW("else")) :: rem =>
                chunk(rem) match
                  case (Some(node), Token(_, KW("end")) :: rest) =>
                    (Some(node), rest)
                  case (_, r) =>
                    err(s"expected else body/end after ${r.head.l}")
              case Token(_, KW("end")) :: rem => (None, rem)
              case _ => err(s"expected `else` or `end` ifs and elseifs")
            (Some(If(cond, body, elifs, elseBlock)), rem)
          case (None, _) =>
            err(s"expected body after `then` after ${cur.head.l}")
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
  def statement(cur: List[Token]): ParseResult = cur match
    case Token(_, KW("local")) :: rest =>
      rest match
        case Token(_, KW("function")) :: _ => funDef(cur)
        case _                             => varDef(cur)
    case Token(l, KW("while")) :: rest => whileLoop(cur)
    case Token(_, KW("for")) :: rest   => forLoop(cur)
    case Token(l, KW("if")) :: rest    => ifBlock(rest)
    case Token(_, ID(name)) :: rest =>
      rest match
        case Token(_, SP("=")) :: _ => varMut(cur)
        case _ =>
          println(s"funcall ${summon[sourcecode.Line]}")
          idExpr(cur)
    case _ => (None, cur)

  def program(cur: List[Token]): TreeNode =
    chunk(cur) match
      case (Some(x), Token(_, Eof) :: Nil) => x
      case other =>
        println(other)
        err(s"expected <eof> after program, got [${other._2.mkString(", ")}]")
end Parser
