package compiler

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import compiler.Inst
import compiler.TreeNode

def asInstr(inst: Int): Inst =
  val op = inst & 0x3f
  val a  = (inst >> 6) & 0xff
  val b  = (inst >> 14) & 0x1ff
  val c  = (inst >> 23) & 0x1ff
  val bx = (inst >> 14) & 0x3ffff
  import Inst.*
  op match
    case 0  => MOVE(a, b)
    case 1  => LOADK(a, bx)
    case 2  => LOADBOOL(a, b, c)
    case 3  => LOADNIL(a, b)
    case 4  => GETUPVAL(a, b)
    case 5  => GETGLOBAL(a, bx)
    case 6  => GETTABLE(a, b, c)
    case 7  => SETGLOBAL(a, bx)
    case 8  => SETUPVAL(a, b)
    case 9  => SETTABLE(a, b, c)
    case 10 => NEWTABLE(a, b, c)
    case 12 => ADD(a, b, c)
    case 13 => SUB(a, b, c)
    case 14 => MUL(a, b, c)
    case 15 => DIV(a, b, c)
    case 16 => MOD(a, b, c)
    case 17 => POW(a, b, c)
    case 18 => UNM(a, b)
    case 19 => NOT(a, b)
    case 20 => LEN(a, b)
    case 21 => CONCAT(a, b, c)
    case 22 => JMP(bx)
    case 23 => EQ(a, b, c)
    case 24 => LT(a, b, c)
    case 25 => LE(a, b, c)
    case 26 => TEST(a, c)
    case 27 => TESTSET(a, b, c)
    case 28 => CALL(a, b)
    case 30 => RETURN(a)
    case 31 => FORLOOP(a, bx)
    case 32 => FORPREP(a, bx)
    case 34 => SETLIST(a, b, c)
    case 35 => CLOSE(a)
    case 36 => CLOSURE(a, bx)
    case _  => throw new Exception(s"Invalid opcode: $op")

object ByteOperations:
  extension (bcStream: ByteArrayInputStream)
    def readInt =
      val b1 = bcStream.read()
      val b2 = bcStream.read()
      val b3 = bcStream.read()
      val b4 = bcStream.read()
      b1 | (b2 << 8) | (b3 << 16) | (b4 << 24)
    def readDouble =
      ByteBuffer.wrap(bcStream.readNBytes(8).reverse).getDouble()
    def readInstr: Inst = asInstr(bcStream.readInt)

object Displayer:
  import ByteOperations.*

  def decompile(bcStream: ByteArrayInputStream, indent: Int): Unit =
    val nup    = bcStream.read()
    val nparam = bcStream.read()
    val nreg   = bcStream.read()
    print(" " * indent)
    println(s"$nup upvalues, $nparam param, $nreg registers needed")
    val ninst = bcStream.readInt
    print(" " * indent)
    println(s"$ninst Insts")
    for _ <- 1 to ninst do
      print(" " * indent)
      println(bcStream.readInstr)
    val nconst = bcStream.readInt
    print(" " * indent)
    println(s"$nconst constants")
    for i <- 1 to nconst do
      val typ = bcStream.read()
      val typeString = typ match
        case 0 => "nil"
        case 1 => "bool"
        case 3 => "number"
        case 4 => "string"
      val value = typ match
        case 0 => TreeNode.LNil
        case 1 => if bcStream.read() == 0 then false else true
        case 3 => bcStream.readDouble
        case 4 =>
          val len = bcStream.readInt
          (1 to len) map (_ => bcStream.read) map (_.toChar) mkString ""
      print(" " * indent)
      println(s"${-i}: $typeString: $value")
    val nfn = bcStream.readInt
    print(" " * indent)
    println(s"$nfn functions")
    println()
    for _ <- 1 to nfn do decompile(bcStream, indent + 1)

  def disp(bytecode: List[Byte]): Unit = decompile(
    ByteArrayInputStream(bytecode.toArray),
    0
  )
