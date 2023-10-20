package compiler

import Instruction.*
import TreeNode.*

case class Chunk(
    instructions: List[Instruction],
    constTable: Map[Double, Int],
    symTable: Map[String, Int],
    upvalTable: Map[String, Int],
    fnTable: List[Chunk],
    paramCnt: Int,
    parent: Chunk = null
):
  // helper functions
  def addInstructions(is: Instruction*): Chunk =
    this.copy(instructions = instructions ++ is)
  def addFn(fn: Chunk): Chunk =
    this.copy(fnTable = fnTable :+ fn)
  def addSymbol(name: String, ind: Int): Chunk =
    assert(symTable.size < 0x100, ">= 256 locals in function")
    this.copy(symTable = symTable + (name -> ind))
  def addUpval(name: String, ind: Int): Chunk =
    this.copy(upvalTable = upvalTable + (name -> ind))
  def setParent(p: Chunk): Chunk =
    this.copy(parent = p)

private object UFlag:
  opaque type UpvalFlag = Boolean
  val LOCAL: UpvalFlag = false
  val UPVAL: UpvalFlag = true
import UFlag.*

object CodeGen:
  private inline def getConst(st: Chunk, value: Double): (Int, Chunk) =
    val consts = st.constTable
    if consts.contains(value) then (consts(value), st)
    else
      val nInd = consts.size + 0x100
      (nInd, st.copy(constTable = consts + (value -> nInd)))

  private inline def findUpval(name: String, parent: Chunk): (UpvalFlag, Int) =
    if parent == null then (UPVAL, -1)
    // in parent symbol table: return that it's local, and that it's a local in the parent
    else if parent.symTable.contains(name) then (LOCAL, parent.symTable(name))
    else if parent.upvalTable.contains(name)
    then // otherwise: check parent upvalue table
      (UPVAL, parent.upvalTable(name))
    else // non-present, add instead. TODO: check whether this works. as it stands, this MUST be coupled with an addUpval call in the parent
      (UPVAL, parent.upvalTable.size)

  private inline def getSym(st: Chunk, name: String): (Int, Chunk, UpvalFlag) =
    if st.symTable.contains(name) then (st.symTable(name), st, LOCAL)
    else if st.upvalTable.contains(name) then (st.upvalTable(name), st, UPVAL)
    else
      val nInd = st.upvalTable.size
      val parent = st.parent
      val (level, symind) = findUpval(name, parent)
      (nInd, st.addUpval(name, nInd), UPVAL)

  private inline def loadValue(
      tree: TreeNode,
      register: Int,
      st: Chunk
  ): Chunk =
    tree match
      case LNum(n) =>
        val (constInd, st2) = getConst(st, n)
        st2.addInstructions(LOADK(register, constInd))
      case Id(name) =>
        val (symInd, st2, flag) = getSym(st, name)
        st2.addInstructions:
          flag match
            case LOCAL => MOVE(register, symInd)
            case UPVAL => GETUPVAL(register, symInd)
      case _ => processExpr(tree, st, register)

  // returns: new state, instructions, operand value (const/reg index), register
  private inline def procssOp(
      tree: TreeNode,
      state: Chunk,
      register: Int
  ): (Chunk, Int, Int) = tree match
    case LNum(value) =>
      val (constInd, st2) = getConst(state, value)
      (st2, constInd, register)
    case Id(name) =>
      val (symInd, st2, flag) = getSym(state, name)
      flag match // if upvalue, prefix w/ getupval, otherwise dierectly use index
        case LOCAL => (st2, symInd, register)
        case UPVAL =>
          (
            st2.addInstructions(GETUPVAL(register, symInd)),
            register,
            register + 1
          )
    case _ => // arbitrary expression
      (processExpr(tree, state, register), register, register + 1)

  private inline def processFunCall(
      name: String,
      args: List[TreeNode],
      state: Chunk,
      regA: Int
  ): Chunk =
    // get register holding function prototype index
    val init = loadValue(Id(name), regA, state)
    // process the arguments: fold w/ state, instruction list, and current register
    val (nst, _) = args.foldLeft(init, regA + 1):
      case ((st, reg), arg) => (processExpr(arg, st, reg), reg + 1)
    nst.addInstructions(CALL(regA, args.size + 1)) // call instruction

  def processExpr(
      tree: TreeNode,
      state: Chunk,
      register: Int
  ): Chunk =
    tree match
      case BinOp(op, left, right) => // process L and R ops
        val (st2, op1, reg1) = procssOp(left, state, register)
        val (st3, op2, _) = procssOp(right, st2, reg1)
        // generate the instruction
        st3.addInstructions:
          op match
            case "+"  => ADD(register, op1, op2)
            case "-"  => SUB(register, op1, op2)
            case "*"  => MUL(register, op1, op2)
            case "/"  => DIV(register, op1, op2)
            case "%"  => MOD(register, op1, op2)
            case "^"  => POW(register, op1, op2)
            case ".." => CONCAT(register, op1, op2)
            case _ =>
              throw Exception(
                s"invalid binary operator ${op} in expression ${tree}"
              )

      case UnOp(op, right) =>
        val (st2, op1, _) = procssOp(right, state, register)
        st2.addInstructions:
          op match
            case "-"   => UNM(register, op1)
            case "not" => NOT(register, op1)
            case _ =>
              throw Exception(
                s"invalid unary operator ${op} in expression ${tree}"
              )
      case FunCall(name, args) => processFunCall(name, args, state, register)
      case LNum(x)             => loadValue(tree, register, state)
      case Id(name)            => loadValue(tree, register, state)
      case _                   => throw Exception(s"invalid expression ${tree}")

  // produce a list of psuedo-instructions (move/getupval) that indicate where the function's nth
  // upvalue is located, either as MOVE 0 X or GETUPVAL 0 X depending on whether it's local to the parent
  private inline def psuedoInstrs(fn: Chunk): List[Instruction] =
    val list = fn.upvalTable.foldLeft(IndexedSeq.fill(fn.upvalTable.size)("")) {
      case (acc, (name, ind)) => acc.updated(ind, name)
    }
    list.foldRight(List[Instruction]()): (name, acc) =>
      val (flag, ind) = findUpval(name, fn.parent)
      flag match
        case UPVAL => GETUPVAL(0, ind) :: acc
        case LOCAL => MOVE(0, ind) :: acc

end CodeGen
