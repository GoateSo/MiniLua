package compiler

import Inst.*
import TreeNode.*
import compiler.Parser.err
import compiler.Parser.comp

// Proto data type to store instructions and relevant info
// also functions as a sort of "state" for the codegen process
// abstract representation of function prototypes
case class Proto(
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

private object UFlag:
  opaque type UpvalFlag = Boolean
  val LOCAL: UpvalFlag = false
  val UPVAL: UpvalFlag = true
import UFlag.*

val ops = Map[String, (Int, Int, Int) => Inst](
  "+"  -> ADD.apply,
  "-"  -> SUB.apply,
  "*"  -> MUL.apply,
  "/"  -> DIV.apply,
  "%"  -> MOD.apply,
  "^"  -> POW.apply,
  ".." -> CONCAT.apply
)

val compares = Map(
  "~=" -> EQ.apply,
  "==" -> EQ.apply,
  "<"  -> LT.apply,
  ">"  -> LT.apply,
  "<=" -> LE.apply,
  ">=" -> LE.apply
)

object CodeGen:
  private inline def getConst(st: Proto, value: Double): (Int, Proto) =
    val consts = st.constTable
    if consts.contains(value) then (consts(value), st)
    else
      val nInd = consts.size + 0x100
      (nInd, st.copy(constTable = consts + (value -> nInd)))

  // finds the upvalue in the parent prototype, checking whether its local to that prototype or its an upval in the parent also
  private inline def findUpval(name: String, par: Proto): (UpvalFlag, Int) =
    if par == null then (UPVAL, -1)
    // in parent symbol table: return that it's local, and that it's a local in the parent
    else if par.symTable.contains(name) then (LOCAL, par.symTable(name))
    // otherwise: check parent upvalue table
    else if par.upvalTable.contains(name) then (UPVAL, par.upvalTable(name))
    // non-present, add instead. TODO: check whether this works. as it stands, this MUST be coupled with an addUpval call in the parent
    else (UPVAL, par.upvalTable.size)

  private inline def getSym(st: Proto, name: String): (Int, Proto, UpvalFlag) =
    if st.symTable.contains(name) then (st.symTable(name), st, LOCAL)
    else if st.upvalTable.contains(name) then (st.upvalTable(name), st, UPVAL)
    else
      val nInd            = st.upvalTable.size
      val parent          = st.parent
      val (level, symind) = findUpval(name, parent)
      (nInd, st.addUpval(name, nInd), UPVAL)

  private inline def loadValue(
      tree: TreeNode,
      register: Int,
      st: Proto
  ): Proto =
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
  private inline def processOp(
      tree: TreeNode,
      state: Proto,
      register: Int
  ): (Proto, Int, Int) = tree match
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
      state: Proto,
      regA: Int
  ): Proto =
    // get register holding function prototype index
    val init = loadValue(Id(name), regA, state)
    // process the arguments: fold w/ state, instruction list, and current register
    val (nst, _) = args.foldLeft(init, regA + 1):
      case ((st, reg), arg) => (processExpr(arg, st, reg), reg + 1)
    nst.addInstructions(CALL(regA, args.size + 1)) // call instruction

  def processExpr(
      tree: TreeNode,
      state: Proto,
      register: Int
  ): Proto =
    tree match
      case BinOp(op, left, right) => // process L and R ops
        // process LHS
        val (st2, op1, reg1) = processOp(left, state, register)
        // generate the instruction
        op match
          // arith operators: process RHS and add instruction on end
          case "+" | "-" | "*" | "/" | "%" | "^" | ".." =>
            val (st3, op2, _) = processOp(right, st2, reg1)
            st3.addInstructions(ops(op)(register, op1, op2))
          // comparison: evalute RHS and then do a comparison + jmp
          // TODO: note: use seperate implementation when it's in a if statement's condition
          case "~=" | "==" | "<" | ">" | "<=" | ">=" =>
            val (st3, op2, _) = processOp(right, st2, reg1)
            val flag = op match
              case "==" | "<" | "<=" => 1
              case _                 => 0
            st3.addInstructions(
              compares(op)(flag, op1, op2), // cmp and jump to load false
              JMP(1),                       // jump to load true instr
              LOADBOOL(register, 0, 1),     // load false and skip 1
              LOADBOOL(register, 1, 0)      // load true
            )
          // logical operators: delay evaluation of RHS until after testing LHS
          // and: jump to end if LHS is false, otherwise evaluate RHS
          // or: jump to end if LHS is true, otherwise evaluate RHS
          case "and" | "or" =>
            ???
          // case
          case _ => err(s"invalid binary operator $op in expression $tree")

      case UnOp(op, right) =>
        val (st2, op1, _) = processOp(right, state, register)
        st2.addInstructions:
          op match
            case "-"   => UNM(register, op1)
            case "not" => NOT(register, op1)
            case _     => err(s"invalid unary operator $op in expression $tree")
      case FunCall(name, args) => processFunCall(name, args, state, register)
      case LNum(x)             => loadValue(tree, register, state)
      case Id(name)            => loadValue(tree, register, state)
      case _                   => throw Exception(s"invalid expression $tree")

  // produce a list of psuedo-instructions (move/getupval) that indicate where the function's nth
  // upvalue is located, either as MOVE 0 X or GETUPVAL 0 X depending on whether it's local to the parent
  private inline def psuedoInstrs(fn: Proto): List[Inst] =
    val list = fn.upvalTable.foldLeft(IndexedSeq.fill(fn.upvalTable.size)("")) {
      case (acc, (name, ind)) => acc.updated(ind, name)
    }
    list.foldRight(List[Inst]()): (name, acc) =>
      val (flag, ind) = findUpval(name, fn.parent)
      flag match
        case UPVAL => GETUPVAL(0, ind) :: acc
        case LOCAL => MOVE(0, ind) :: acc

end CodeGen
