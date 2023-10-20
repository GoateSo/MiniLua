package utils

import compiler.TreeNode

object Parseutil:
  def asString(node: TreeNode): String = node match
    case TreeNode.BinOp(op, left, right) =>
      s"(${asString(left)} $op ${asString(right)})"
    case TreeNode.UnOp(op, right) =>
      s"($op${asString(right)})"
    case TreeNode.VarDef(name, value) =>
      s"local $name = ${asString(value)}"
    case TreeNode.VarMut(name, value) =>
      s"$name = ${asString(value)}"
    case TreeNode.FunDef(name, args, body) =>
      s"local function $name(${args.mkString(", ")}) ${asString(body).indent(2)}end"
    case TreeNode.FunCall(name, args) =>
      s"$name(${args.map(asString).mkString(", ")})"
    case TreeNode.While(cond, body) =>
      s"while ${asString(cond).indent(1)} do\n${asString(body).indent(2)}\nend"
    case TreeNode.Break => "break"
    case TreeNode.For(name, start, end, step, body) =>
      s"for $name=${asString(start)}, ${asString(end)}${step
          .map(s => s", ${asString(s)}")
          .getOrElse("")} do${asString(body).indent(2)}end"
    case TreeNode.If(cond, body, elifs, elseBody) =>
      s"if ${asString(cond)} then${asString(body).indent(2)}${elifs
          .map(e => s"elseif ${asString(e._1)} then${asString(e._2).indent(2)}")
          .mkString("")}${elseBody.map(e => s"else${asString(e).indent(2)}").getOrElse("")}end"
    case TreeNode.Chunk(stmts) =>
      println("a")
      s"${stmts.map(asString(_)).mkString("\n", "\n", "")}"
    case TreeNode.LNum(value)  => s"$value"
    case TreeNode.LBool(value) => s"$value"
    case TreeNode.LStr(value)  => s"$value"
    case TreeNode.LNil         => "nil"
    case TreeNode.Id(name)     => s"$name"
    case TreeNode.Arr(fields)  => s"{${fields.map(asString).mkString(", ")}}"
    case TreeNode.Return(expr) => s"return ${asString(expr)}"
    case TreeNode.TInd(tab, ind) =>
      s"(${asString(tab)})[${asString(ind)}]"
