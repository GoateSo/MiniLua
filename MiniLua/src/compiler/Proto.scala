package compiler
// Proto data type to store instructions and relevant info
// also functions as a sort of "state" for the codegen process
// abstract representation of function prototypes
final case class Proto(
    instructions: List[Inst],
    constTable: Map[Double, Int],
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
