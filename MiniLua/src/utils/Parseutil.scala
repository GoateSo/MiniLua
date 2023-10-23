package utils

import compiler.TreeNode
import TreeNode.*

object Parseutil:
  def asString(node: TreeNode): String = node match
    case VarDef(name, value) =>
      s"local $name = ${asString(value)}"
    case VarMut(name, value) =>
      s"$name = ${asString(value)}"
    case FunDef(name, args, body) =>
      s"local function $name(${args.mkString(", ")}) ${asString(body).indent(2)}end"
    case FunCall(name, args) =>
      s"$name(${args.map(asString).mkString(", ")})"
    case While(cond, body) =>
      s"while ${asString(cond)} do${asString(body).indent(2)}end"
    case Break => "break"
    case For(name, start, end, step, body) =>
      s"for $name=${asString(start)}, ${asString(end)}${step
          .map(s => s", ${asString(s)}")
          .getOrElse("")} do${asString(body).indent(2)}end"
    case If(cond, body, elifs, elseBody) =>
      s"if ${asString(cond)} then${asString(body).indent(2)}${elifs
          .map(e => s"elseif ${asString(e._1)} then${asString(e._2).indent(2)}")
          .mkString("")}${elseBody.map(e => s"else${asString(e).indent(2)}").getOrElse("")}end"
    case Chunk(stmts) =>
      println("a")
      s"${stmts.map(asString(_)).mkString("\n", "\n", "")}"
    // expression sub-nodes
    case BinOp(op, left, right) =>
      s"(${asString(left)} $op ${asString(right)})"
    case UnOp(op, right) =>
      s"($op${asString(right)})"
    case LNum(value)  => s"$value"
    case LBool(value) => s"$value"
    case LStr(value)  => s"$value"
    case LNil         => "nil"
    case Id(name)     => s"$name"
    case Arr(fields)  => s"{${fields.map(asString).mkString(", ")}}"
    case Return(expr) => s"return ${asString(expr)}"
    case TInd(tab, ind) =>
      s"(${asString(tab)})[${asString(ind)}]"
