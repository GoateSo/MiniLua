package compiler

/**
 * AST treenodes, specified by grammer in <parser.scala>
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
      step: TreeNode,
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
