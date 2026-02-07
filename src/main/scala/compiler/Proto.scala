package compiler
import TreeNode.*
type LVal = Double | String | Boolean | LNil.type
// Proto data type to store instructions and relevant info
// also functions as a sort of "state" for the codegen process
// abstract representation of function prototypes
final case class Proto(
    instructions: List[Inst],
    constTable: Map[LVal, Int],
    symTable: Map[String, Int],
    upvalTable: Map[String, Int],
    fnTable: List[Proto],
    paramCnt: Int,
    parent: Proto = null
):
  // helper functions
  def addInstructions(is: Inst*): Proto =
    this.copy(instructions = instructions ++ is)
  def addFn(fn: Proto): Proto =
    this.copy(fnTable = fnTable :+ fn)
  def addSymbol(name: String, ind: Int): Proto =
    assert(symTable.size < 256, ">= 256 locals in function")
    this.copy(symTable = symTable + (name -> ind))
  def addUpval(name: String, ind: Int): Proto =
    this.copy(upvalTable = upvalTable + (name -> ind))
  def setParent(p: Proto): Proto =
    this.copy(parent = p)
  def hasLocal(name: String): Boolean =
    symTable.contains(name)
  def hasUpval(name: String): Boolean =
    upvalTable.contains(name)
  override def toString(): String =
    val instrs =
      s"Instructions:\n${instructions.zipWithIndex.map((a, b) => s"$b\t:$a").mkString("\n")}"
    val consts =
      s"Constants:\n${constTable.toSeq.sortBy(_._2).map((a, b) => s"$b\t$a").mkString("\n")}"
    val symbols =
      s"Symbols:\n${symTable.toSeq.sortBy(_._2).map((a, b) => s"$b\t$a").mkString("\n")}"
    val upvals =
      s"Upvalues:\n${upvalTable.toSeq.sortBy(_._2).map((a, b) => s"$b\t$a").mkString("\n")}"
    val fns =
      s"Functions:\n${fnTable.map(p => p.toString().indent(2)).mkString("\n")}"
    val pCnt =
      s"Parameter Count: $paramCnt"

    s"[proto]\n$pCnt\n$instrs\n$consts\n$symbols\n$upvals\n$fns"
