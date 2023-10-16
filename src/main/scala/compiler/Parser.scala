package compiler

import compiler.Tokenizer.Token
import compiler.Tokenizer.Location
import TokenType.*
import compiler.Tokenizer.err
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
  case VarDef(name: String, value: TreeNode)
  case VarMut(name: String, value: TreeNode)
  case FunDef(name: String, args: List[String], body: TreeNode)
  case FunCall(name: String, args: List[TreeNode])
  case While(cond: TreeNode, body: TreeNode)
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
      elseBody: Option[TreeNode] // else block
  )
  case LNumber(value: Double)
  case LBool(value: Boolean)
  case LString(value: String)
  case LNil
  case Id(name: String)
  case Arr(fields: List[TreeNode])
  case Return(expr: TreeNode)
  case Program(stmts: List[TreeNode])

object Parser:
  import TreeNode.*

  type ParseResult = (Option[TreeNode], List[Token])
  inline def rmap(f: TreeNode => TreeNode)(res: ParseResult): ParseResult =
    res match
      case (Some(node), rest) => (Some(f(node)), rest)
      case other              => other

  inline def err(msg: String) = throw Exception(s"parse error: $msg")

  def factor(cur: List[Token]): ParseResult =
    cur match
      case Nil => (None, Nil)
      case Token(_, Ident(name)) ::
          Token(l2, Special("(")) :: rest =>
        val (args, rest2) = exprList(rest)
        (Some(FunCall(name, args)), rest2)
      case Token(_, Special("(")) :: rest =>
        val (exp, rest2) = add(rest)
        rest2 match
          case Token(_, Special(")")) :: rest3 =>
            (exp, rest3)
          case _ =>
            err(s"expected ')' at $rest2")
      // literals
      case Token(_, Ident(name)) :: rest =>
        (Some(Id(name)), rest)
      case Token(_, Num(value)) :: rest =>
        (Some(LNumber(value.toDouble)), rest)
      case Token(_, Str(value)) :: rest =>
        (Some(LString(value)), rest)
      case Token(_, Word("nil")) :: rest =>
        (Some(LNil), rest)
      case Token(_, Word("true")) :: rest =>
        (Some(LBool(true)), rest)
      case Token(_, Word("false")) :: rest =>
        (Some(LBool(false)), rest)
      case Token(_, Special("{")) :: rest =>
        val (fields, rest2) = fieldList(rest)
        (Some(Arr(fields)), rest2)
      case _ => // soft fail
        (None, cur)
  def power(cur: List[Token]): ParseResult =
    factor(cur) match
      case (Some(left), Token(_, Special("^")) :: rest) =>
        rmap(BinOp("^", left, _))(power(rest))
      case other => other

  def unop(cur: List[Token]): ParseResult =
    cur match
      case Token(_, Special(op)) :: next if op == "-" || op == "#" =>
        rmap(UnOp(op, _))(unop(next))
      case _ => power(cur)

  private def tailGo(
      node: TreeNode,
      cur: List[Token],
      ops: Set[String],
      parser: List[Token] => ParseResult
  ): (Option[TreeNode], List[Token]) =
    cur match
      case Token(_, Special(op)) :: rest if ops.contains(op) =>
        parser(rest) match
          case (Some(right), rest2) =>
            tailGo(BinOp(op, node, right), rest2, ops, parser)
          case other => (Some(node), other._2)
      case other => (Some(node), other)

  // ops w/ same precedence as multiplication
  def mul(cur: List[Token]): ParseResult =
    unop(cur) match
      case (Some(left), rest) => tailGo(left, rest, Set("*", "/", "%"), unop)
      case other              => other

  // ops w/ same precedence as addition
  def add(cur: List[Token]): ParseResult =
    mul(cur) match
      case (Some(left), rest) => tailGo(left, rest, Set("+", "-"), mul)
      case other              => other

  def concat(cur: List[Token]): ParseResult =
    add(cur) match
      case (Some(left), rest) => tailGo(left, rest, Set(".."), add)
      case other              => other

  def comp(cur: List[Token]): ParseResult =
    concat(cur) match
      case (Some(left), rest) =>
        tailGo(left, rest, Set("<", "<=", ">", ">=", "~=", "=="), concat)
      case other => other

  def and(cur: List[Token]): ParseResult =
    comp(cur) match
      case (Some(left), rest) => tailGo(left, rest, Set("and"), comp)
      case other              => other

  def or(cur: List[Token]): ParseResult =
    and(cur) match
      case (Some(left), rest) => tailGo(left, rest, Set("or"), and)
      case other              => other

  // alias for expression, as or is currently op w/ lowest precedence
  val expr = or

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
      case (Some(node), Token(_, Special(`term`)) :: rest) =>
        (node :: ret, rest) // terminated
      case (Some(node), Token(_, Special(s)) :: rest) if seps(s) =>
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
  
end Parser
