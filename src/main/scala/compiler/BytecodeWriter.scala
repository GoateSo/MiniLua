package compiler

import java.nio.ByteBuffer
import Inst.*
import CodeGen.*
import scala.collection.immutable.Queue
import java.io.File
import java.io.FileOutputStream
/*
-- Notations:

R: registers
C: constants
RC: either regist or constant

A : register A
Ax: register A extended

iABC format:
  Op: bits 0-5
  A: bits 6-13
  B: bits 14-22
  C: bits 23-31
iABx format:
  Op: bits 0-5
  A: bits 6-13
  Bx: bits 14-31

extended signed register (sBx) unused b/c JMP and FORLOOP instructions aren't present
 */
object BytecodeWriter:
  // each instruction corresponds to 4 byte segment in bytecode

  // iABC format
  // CCCCCCCCCBBBBBBBBBAAAAAAAAOOOOOO
  // 9 bit- 9 bit- 8 bit- 6 bit
  private inline def toInst(inline op: Int, a: Int, b: Int, c: Int): Int =
    (op & 0x3f) | ((a & 0xff) << 6) | ((b & 0x1ff) << 14) | ((c & 0x1ff) << 23)
  // iABx format
  // BBBBBBBBBBBBBBBBBAAAAAAAAOOOOOO
  // 18 bit- 8 bit- 6 bit
  private inline def toInst(inline op: Int, a: Int, bx: Int): Int =
    (op & 0x3f) | ((a & 0xff) << 6) | ((bx & 0x3ffff) << 14)

  private inline def constBytes(c: Double): Array[Byte] =
    ByteBuffer
      .allocate(8)
      .putDouble(c)
      .array()
      .reverse // maybe not needed?

  private inline def constBytes(c: Int): List[Byte] =
    List(
      (c & 0xff).toByte,
      ((c >> 8) & 0xff).toByte,
      ((c >> 16) & 0xff).toByte,
      ((c >> 24) & 0xff).toByte
    ) // .reverse?

  extension (buf: Queue[Byte])
    private def writeByte(b: Byte): Queue[Byte] = buf.enqueue(b)
    // little write int:
    private def writeInt(i: Int): Queue[Byte] =
      buf.enqueueAll(constBytes(i))
    private def writeInstrs(xs: List[Inst]): Queue[Byte] =
      xs.foldLeft(buf)((buf, x) => buf.writeInt(x match
        case MOVE(a, b) => toInst(x.code, a, b, 0)
        case LOADK(a, bx) => toInst(x.code, a, bx)
        case LOADBOOL(a, b, c) => toInst(x.code, a, b, c)
        case LOADNIL(a, b) => toInst(x.code, a, b, 0)
        case GETUPVAL(a, b) => toInst(x.code, a, b, 0)
        case GETGLOBAL(a, bx) => toInst(x.code, a, bx)
        case GETTABLE(a, b, c) => toInst(x.code, a, b, c)
        case SETGLOBAL(a, bx) => toInst(x.code, a, bx)
        case SETUPVAL(a, b) => toInst(x.code, a, b, 0)
        case SETTABLE(a, b, c) => toInst(x.code, a, b, c)
        case NEWTABLE(a, b, c) => toInst(x.code, a, b, c)
        case ADD(a, b, c) => toInst(x.code, a, b, c)
        case SUB(a, b, c) => toInst(x.code, a, b, c)
        case MUL(a, b, c) => toInst(x.code, a, b, c)
        case DIV(a, b, c) => toInst(x.code, a, b, c)
        case MOD(a, b, c) => toInst(x.code, a, b, c)
        case POW(a, b, c) => toInst(x.code, a, b, c)
        case UNM(a, b) => toInst(x.code, a, b, 0)
        case NOT(a, b) =>  toInst(x.code, a, b, 0)
        case LEN(a, b) =>  toInst(x.code, a, b, 0)
        case CONCAT(a, b, c) =>  toInst(x.code, a, b, c)
        case JMP(sBx) => toInst(x.code, 0, sBx)
        case EQ(a, b, c) => toInst(x.code, a, b, c)
        case LT(a, b, c) => toInst(x.code, a, b, c)
        case LE(a, b, c) => toInst(x.code, a, b, c)
        case TEST(a, c) => toInst(x.code, a, 0, c)
        case TESTSET(a, b, c) => toInst(x.code, a, b, c)
        case CALL(a, b) => toInst(x.code, a, b, 0)
        case RETURN(a) => toInst(x.code, a, 0, 0)
        case FORLOOP(a, sBx) => toInst(x.code, a, sBx)
        case FORPREP(a, sBx) => toInst(x.code, a, sBx)
        case SETLIST(a, b, c) => toInst(x.code, a, b, c)
        case CLOSE(a) => toInst(x.code, a, 0, 0)
        case CLOSURE(a, bx) => toInst(x.code, a, bx)
      ))

    private def writeConsts(xs: List[LVal]): Queue[Byte] =
      xs.foldLeft(buf)((buf, x) =>
        buf.enqueueAll(
          x match
            case TreeNode.LNil => List[Byte](0)
            case _: Boolean =>
              List[Byte](1, if x == true then 1 else 0)
            case d: Double => 3 :: constBytes(d).toList
            case s: String => 4 :: constBytes(s.size) ::: s.getBytes.toList
        )
      )

  private inline def getMaxRegister(c: Proto): Int =
    // hacky soln to get the # of regs needed
    c.instructions.foldLeft(0)((acc, x) =>
      math.max(x.productElement(0).asInstanceOf[Int], acc)
    )
  // Chunk as top level function block
  // Int: line defined
  // Int: last line defined
  // 1 byte: number of upvalues
  // 1 byte: number of parameters
  // 1 byte: stack size (# registers used)
  // List of instructions
  // List of constants
  // List of functions
  // List of locals
  // List of upvalues
  // TODO: record upvalues with level and register index
  def toByteStream(program: Proto): Queue[Byte] =
    val Proto(
      instructions,
      constTable,
      symTable,
      upvalTable,
      fnTable,
      paramCnt,
      _
    ) = program
    val list = Queue
      .empty[Byte] // preamble
      .writeByte(upvalTable.size.toByte)
      .writeByte(paramCnt.toByte)
      .writeByte((getMaxRegister(program) + 1).toByte)
      // instructions
      .writeInt(instructions.size)
      .writeInstrs(instructions)
      // constants
      .writeInt(constTable.size)
      .writeConsts(
        constTable.toList.sortBy(_._2).map(_._1)
      )
      // functions
      .writeInt(fnTable.size)
    // write function impls
    fnTable.foldLeft(list)((buf, fn) =>
      val dumped = toByteStream(fn)
      buf.enqueueAll(dumped)
    )

  def writeToFile(program: Proto, fileName: String = "bytecode.out") =
    val bytes  = toByteStream(program)
    val file   = File(fileName)
    val output = FileOutputStream(file)
    output.write(bytes.toArray)

end BytecodeWriter
