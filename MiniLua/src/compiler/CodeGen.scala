package compiler

import Inst.*
import TreeNode.*
import compiler.Parser.err
import compiler.Parser.comp
import utils.GenUtils
import utils.Parseutil

private object UFlag:
  opaque type UpvalFlag = Boolean
  val LOCAL: UpvalFlag = false
  val UPVAL: UpvalFlag = true
import UFlag.*

// TODO: possible changes
// - dont pass existing instructions as part of param; only pass other state to limit iteration when subbing placeholder jmps
// - change placeholder value for break and add checks that it's not present outisde loops
object CodeGen:
  private inline def isConst(t: TreeNode) =
    t.isInstanceOf[LNum | LStr | LBool | LNil.type]
  private inline def toInt(v: Boolean) =
    if v then 1 else 0
  private inline def getLVal(t: TreeNode): LVal =
    t match
      case LNil     => LNil
      case LNum(n)  => n
      case LStr(s)  => s
      case LBool(b) => b
      case _        => err("invalid constant")

  private inline def getConst(st: Proto, value: LVal): (Int, Proto) =
    val consts = st.constTable
    if consts.contains(value) then (consts(value), st)
    else
      val nInd = consts.size + 0x100
      (nInd, st.copy(constTable = consts + (value -> nInd)))

  private inline def getSym(st: Proto, name: String): (Int, Proto, UpvalFlag) =
    if st.symTable.contains(name) then (st.symTable(name), st, LOCAL)
    else if st.upvalTable.contains(name) then (st.upvalTable(name), st, UPVAL)
    else // TODO: fix this dumb code
      val nInd = st.upvalTable.size
      (nInd, st.addUpval(name, nInd), UPVAL)

  private def flattenOp(op: String, root: TreeNode): List[TreeNode] =
    root match
      case BinOp(op2, left, right) if op == op2 =>
        flattenOp(op, left) ++ flattenOp(op, right)
      case _ => List(root)

  private inline def endJmps(st: Proto, placeHolder: Int = -1): Proto =
    st.copy:
      st.instructions.zipWithIndex.map:
        case (JMP(`placeHolder`), i) => JMP(st.instructions.size - i - 1)
        case (other, i)              => other

  private inline def loadValue(
      tree: TreeNode,
      register: Int,
      st: Proto
  ): Proto =
    tree match
      case LNil =>
        st.addInstructions(LOADNIL(register, 0))
      case LBool(value) =>
        st.addInstructions(LOADBOOL(register, toInt(value), 0))
      case LStr(s) =>
        val (constInd, st2) = getConst(st, s)
        st2.addInstructions(LOADK(register, constInd))
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
      reg: Int
  ): (Proto, Int, Int) = tree match
    // TODO: flesh out processing of constants (esp new ones (nil, bool, str))
    // explicitly put in constant table and get its index in the const table; doesn't use instruction
    case _ if isConst(tree) =>
      val (constInd, st2) = getConst(state, getLVal(tree))
      (st2, constInd, reg)
    // tries to find in symtable, otherwise emit a getupval instruction
    case Id(name) =>
      val (symInd, st2, flag) = getSym(state, name)
      flag match // if upvalue, prefix w/ getupval, otherwise dierectly use index
        case LOCAL => (st2, symInd, reg)
        case UPVAL =>
          (st2 addInstructions GETUPVAL(reg, symInd), reg, reg + 1)
    // eval as arbitrary expression if not a const/variable
    case _ => // arbitrary expression
      (processExpr(tree, state, reg), reg, reg + 1)

  // processes a function call (name and args) into a list of instructions
  private inline def processFCall(
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
    nst addInstructions CALL(regA, args.size + 1) // call instruction

  // very simple impl of boolean event processing
  // -2 for jumps on true, -1 for jumps on false
  private def processBoolExpr(
      tree: TreeNode,
      state: Proto,
      reg: Int
  ): Proto = tree match
    case BinOp(op, left, right) if op == "and" || op == "or" =>
      // 0 => jump on false, 1 => jump on true
      flattenOp(op, tree) match
        // 2. join w/ jmp instructions as seperators
        case rest :+ last =>
          // process all but last subexpression, w/ special rules adding Test/Testset; add jmp(-1) at end of each instr
          val flag = if op == "and" then 0 else 1
          val res = rest.foldLeft(state):
            case (st, sub) =>
              sub match
                case Id(name) if st.hasLocal(name) =>
                  st.addInstructions(
                    TESTSET(reg, st.symTable(name), flag),
                    JMP(-1)
                  )
                case _ => // arbitrary expression, process and add test
                  // TODO: figure out case for mixture of "and"s and "or"s
                  processExpr(sub, st, reg)
                    .addInstructions(TEST(reg, flag), JMP(-1))
          // process last subexpression as normal
          processExpr(last, res, reg)
        case _ => err("impossible case")
    case _ => err("internal err: invalid boolean expression")

  /**
   * parses an expression tree into a list of instructions in a given context
   */
  def processExpr(
      tree: TreeNode,
      state: Proto,
      register: Int
  ): Proto =
    tree match
      case FunCall(name, args) => processFCall(name, args, state, register)
      case c if isConst(c)     => loadValue(tree, register, state)
      case Id(name)            => loadValue(tree, register, state)
      case TInd(tab, ind) =>
        val (st2, tLoc, _) = processOp(tab, state, register)
        val (st3, iLoc, _) = processOp(ind, st2, register + 1)
        st3.addInstructions(GETTABLE(register, tLoc, iLoc))
      case Arr(fields) =>
        val st2 = state.addInstructions(NEWTABLE(register, fields.size, 0))
        val (st3, _) = fields.foldLeft(st2, register + 1):
          case ((st, reg), value) =>
            val (nst, op, _) = processOp(value, st, reg)
            (nst.addInstructions(LOADK(reg, op)), reg + 1)
        st3.addInstructions(
          SETLIST(register, fields.size, 1)
        )
      case UnOp(op, right) =>
        val (st2, op1, _) = processOp(right, state, register)
        st2.addInstructions:
          op match
            case "-"   => UNM(register, op1)
            case "not" => NOT(register, op1)
            case _ =>
              err(
                s"invalid unary operator $op in expression ${Parseutil.asString(tree)}"
              )
      case BinOp(op, left, right) =>
        op match
          // arith operators: process RHS and add instruction on end
          case ".." => // concat loads both operations explicitly
            val st2 = processExpr(left, state, register)
            val st3 = processExpr(right, st2, register + 1)
            st3.addInstructions(CONCAT(register, register, register + 1))
          case "+" | "-" | "*" | "/" | "%" | "^" =>
            val (st2, op1, reg1) = processOp(left, state, register)
            val (st3, op2, _)    = processOp(right, st2, reg1)
            st3.addInstructions(GenUtils.ops(op)(register, op1, op2))
          // comparison: evalute RHS and then do a comparison + jmp
          // TODO: note: use seperate implementation when it's in a if statement's condition
          case "~=" | "==" | "<" | ">" | "<=" | ">=" =>
            val (st2, op1, reg1) = processOp(left, state, register)
            val (st3, op2, _)    = processOp(right, st2, reg1)
            val flag = op match // negation flags (e.g. eq vs neq)
              case "==" | "<" | "<=" => 1
              case _                 => 0
            st3.addInstructions(
              GenUtils
                .compares(op)(flag, op1, op2), // cmp and jump to load false
              JMP(1),                          // jump to load true instr
              LOADBOOL(register, 0, 1),        // load false and skip 1
              LOADBOOL(register, 1, 0)         // load true
            )
          // logical operators: delay evaluation of RHS until after testing LHS
          case "and" | "or" =>
            val opList = processBoolExpr(tree, state, register)
            // 3. figure out jmp distances and replace placeholders
            endJmps(opList)
          case _ => err(s"invalid binary operator $op in expression $tree")
      case _ => throw Exception(s"invalid expression $tree")

  // finds the upvalue in the parent prototype, checking whether its local to that prototype or its an upval in the parent also
  private inline def findUpval(name: String, par: Proto): (UpvalFlag, Int) =
    if par == null then (UPVAL, -1)
    // in parent symbol table: return that it's local, and that it's a local in the parent
    else if par.symTable.contains(name) then (LOCAL, par.symTable(name))
    // otherwise: check parent upvalue table
    else if par.upvalTable.contains(name) then (UPVAL, par.upvalTable(name))
    // non-present, add instead. TODO: check whether this works. as it stands, this MUST be coupled with an addUpval call in the parent
    else (UPVAL, par.upvalTable.size)

  // produce a list of psuedo-instructions (move/getupval) that indicate where the function's nth
  // upvalue is located, either as MOVE 0 X or GETUPVAL 0 X depending on whether it's local to the parent
  private inline def psuedoInstrs(fn: Proto): List[Inst] =
    val list = fn.upvalTable.foldLeft(IndexedSeq.fill(fn.upvalTable.size)("")):
      case (acc, (name, ind)) => acc.updated(ind, name)
    list.foldRight(List[Inst]()): (name, acc) =>
      val (flag, ind) = findUpval(name, fn.parent)
      flag match
        case UPVAL => GETUPVAL(0, ind) :: acc
        case LOCAL => MOVE(0, ind) :: acc
  // variable assignment, either declaration or mutation
  // translate the definition and add to symbol table
  private inline def varAssign(
      name: String,
      defn: TreeNode,
      register: Int,
      st: Proto
  ): Proto = loadValue(defn, register, st).addSymbol(name, register)

  private inline def mkClosure(
      args: List[String],
      body: TreeNode,
      parent: Proto
  ) = Proto(
    instructions = Nil,
    constTable = Map.empty,
    symTable = args.zipWithIndex.toMap, // parameters
    upvalTable = Map.empty,
    fnTable = Nil,
    paramCnt = args.size, // param cnt
    parent = parent       // store parent as well
  )

  def processStmt(
      state: Proto,
      tree: TreeNode
  ): Proto = tree match
    case Chunk(stmts) => stmts.foldLeft(state)(processStmt)
    case VarDef(name, value) =>
      varAssign(name, value, state.symTable.size, state)
    case VarMut(name, value) =>
      varAssign(name, value, state.symTable(name), state)
    case FunCall(name, args) =>
      processFCall(name, args, state, state.symTable.size)
    case Break /*placeholder jmp*/ => state.addInstructions(JMP(-1))
    case FunDef(name, args, body)  =>
      // add function itself as a local variable
      val parState = state.addSymbol(name, state.symTable.size)
      // TODO: possibly take 2nd look at this - parse body as chunk w/ custom proto
      val funbody = processStmt(
        mkClosure(args, body, parState),
        body
      ).addInstructions(RETURN(args.size))
      // create closure instruction, and update state (local var + fn table)
      val afterClosure = parState
        .addFn(funbody)
        .addInstructions(
          (CLOSURE(state.symTable.size, state.fnTable.size)
            :: psuedoInstrs(funbody)): _*
        )
      // add upvalues to parent function(s), in order of their index
      funbody.upvalTable.toList
        .sortBy(_._2)
        .foldLeft(afterClosure):
          case (st, (name, ind)) => // if upvalue not present, add to upvaltable
            if st.symTable.contains(name) || st.upvalTable.contains(name)
            then st
            else st.addUpval(name, st.upvalTable.size)
    case While(cond, body) =>
      // evaluate condition, add jump-if-false condition
      // evaluate loop body, add unconditional jump to condition
      // sub -1 => end of loop, sub -2 => start of loop
      val preSize = state.instructions.size
      val condCode = processExpr(cond, state, state.symTable.size)
        .addInstructions(TEST(state.symTable.size, 0), JMP(-1))
      val bodyCode = processStmt(condCode, body).addInstructions(JMP(-2))
      bodyCode
        .copy( // substitute all "jump to end" instructions w/ correct offset
          instructions = bodyCode.instructions.zipWithIndex.map:
            case (JMP(-1), i) => JMP(bodyCode.instructions.size - i - 1)
            case (JMP(-2), i) => JMP(preSize - i - 1)
            case (other, _)   => other
          ,
          symTable = state.symTable
        )
    case For(name, start, end, step, body) =>
      // starting line and register -- for jumps
      val baseReg   = state.symTable.size
      val startLine = state.instructions.size + 2
      // load start, end, and step, and the iterator variable
      val h1 = loadValue(start, baseReg, state)
      val h2 = loadValue(end, baseReg + 1, h1)
      val h3 = loadValue(step, baseReg + 2, h2)
        .addSymbol(name, baseReg + 3)
      val st = processStmt(
        h3.addInstructions(FORPREP(baseReg, -1)).addSymbol(name, baseReg + 3),
        body
      ).addInstructions(FORLOOP(baseReg, -1))
      // substitute all "jump to end" and "jump to start" instructions w/ correct offset
      st.copy(
        // replace placeholders w/ correct offsets
        // forprep goes to forloop instr
        // jmp to end in case of break stmt
        // forloop goes to instr below forprep
        instructions = st.instructions.zipWithIndex.map:
          case (FORPREP(x, -1), i) => FORPREP(x, st.instructions.size - i - 2)
          case (JMP(-1), i)        => JMP(st.instructions.size - i - 1)
          case (FORLOOP(x, -1), i) => FORLOOP(x, startLine - i + 1)
          case (other, _)          => other
        , // also restore the inital symtable
        symTable = state.symTable
      )
    case If(cond, body, elifs, elseBody) =>
      val conds = (cond, body) :: elifs
      val nst = conds.foldLeft(state):
        case (st, (cond, body)) =>
          // add in the expressions, with the if-statement test+jmp
          val nst = processExpr(cond, st, st.symTable.size)
            .addInstructions(TEST(st.symTable.size, 0), JMP(-1))
          // jump statement to skip to either end or else block
          // then clear out variables declared in the body
          endJmps(
            processStmt(nst, body)
              .addInstructions(JMP(-2)) // jmp to end
              .copy(symTable = st.symTable)
          )
      val res = elseBody match
        case Some(eb) => processStmt(nst, eb)
        case _        => nst
      // substitute all "jump to end" instructions w/ correct offset
      endJmps(res, -2)
    case Return(expr) =>
      expr match
        case Id(name) =>
          val reg              = state.symTable.size
          val (ind, nst, flag) = getSym(state, name)
          flag match
            case LOCAL => nst.addInstructions(RETURN(ind))
            case UPVAL => nst.addInstructions(GETUPVAL(reg, ind), RETURN(reg))
        case _ =>
          val reg = state.symTable.size
          val nst = loadValue(expr, reg, state)
          nst addInstructions RETURN(reg)
    case _ => err("invalid statement")

end CodeGen
