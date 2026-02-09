package readerWriterTests

import munit.*
import compiler.*
import compiler.Inst.*
import compiler.TreeNode.*
import java.io.ByteArrayOutputStream
import java.io.PrintStream

class ReaderWriterSuite extends FunSuite:

  test("BytecodeWriter and Displayer simple constant") {
    val proto = Proto(
      instructions = List(LOADK(0, 0)),
      constTable = Map(1.0 -> 0),
      symTable = Map.empty,
      upvalTable = Map.empty,
      fnTable = Nil,
      paramCnt = 0
    )

    val bytes = BytecodeWriter.toByteStream(proto)
    val byteList = bytes.toList

    // Verify header
    assertEquals(byteList(0), 0.toByte)
    assertEquals(byteList(1), 0.toByte)
    assertEquals(byteList(2), 1.toByte)

    // Verify instructions size (1 -> 4 bytes)
    assertEquals(byteList.slice(3, 7), List[Byte](1, 0, 0, 0))

    // Verify instruction (LOADK 0 0)
    assertEquals(byteList.slice(7, 11), List[Byte](1, 0, 0, 0))

    // Verify constants size (1 -> 4 bytes)
    assertEquals(byteList.slice(11, 15), List[Byte](1, 0, 0, 0))

    // Verify constant (1.0)
    assertEquals(byteList(15), 3.toByte)
    val expectedDoubleBytes = List[Byte](0, 0, 0, 0, 0, 0, 0xf0.toByte, 0x3f.toByte)
    assertEquals(byteList.slice(16, 24), expectedDoubleBytes)

    // Verify functions size (0 -> 4 bytes)
    assertEquals(byteList.slice(24, 28), List[Byte](0, 0, 0, 0))

    // Test Displayer
    val stream = new ByteArrayOutputStream()
    val printStream = new PrintStream(stream)
    Displayer.disp(byteList, printStream)
    printStream.flush()
    val output = stream.toString

    val normalizedOutput = output.replace("\r\n", "\n")

    val expectedOutput = 
      """0 upvalues, 0 param, 1 registers needed
        |1 Insts
        |LOADK(0,0)
        |1 constants
        |-1: number: 1.0
        |0 functions
        |
        |""".stripMargin

    assertNoDiff(normalizedOutput, expectedOutput)
  }

  test("BytecodeWriter and Displayer nested function") {
    val innerProto = Proto(
      instructions = List(RETURN(0)),
      constTable = Map.empty,
      symTable = Map.empty,
      upvalTable = Map.empty,
      fnTable = Nil,
      paramCnt = 0
    )

    val outerProto = Proto(
      instructions = List(CLOSURE(0, 0)),
      constTable = Map.empty,
      symTable = Map.empty,
      upvalTable = Map.empty,
      fnTable = List(innerProto),
      paramCnt = 0
    )

    val bytes = BytecodeWriter.toByteStream(outerProto)
    val byteList = bytes.toList

    // Verify outer function structure
    assertEquals(byteList.slice(0, 3), List[Byte](0, 0, 1))
    assertEquals(byteList.slice(3, 7), List[Byte](1, 0, 0, 0))
    assertEquals(byteList.slice(7, 11), List[Byte](36, 0, 0, 0))
    assertEquals(byteList.slice(11, 15), List[Byte](0, 0, 0, 0))
    assertEquals(byteList.slice(15, 19), List[Byte](1, 0, 0, 0))

    val innerStart = 19
    assertEquals(byteList.slice(innerStart, innerStart + 3), List[Byte](0, 0, 1))
    assertEquals(byteList.slice(innerStart + 3, innerStart + 7), List[Byte](1, 0, 0, 0))
    assertEquals(byteList.slice(innerStart + 7, innerStart + 11), List[Byte](30, 0, 0, 0))

    // Test Displayer
    val stream = new ByteArrayOutputStream()
    val printStream = new PrintStream(stream)
    Displayer.disp(byteList, printStream)
    printStream.flush()
    val output = stream.toString

    val normalizedOutput = output.replace("\r\n", "\n")

    val expectedOutput =
      """0 upvalues, 0 param, 1 registers needed
        |1 Insts
        |CLOSURE(0,0)
        |0 constants
        |1 functions
        |
        | 0 upvalues, 0 param, 1 registers needed
        | 1 Insts
        | RETURN(0)
        | 0 constants
        | 0 functions
        |
        |""".stripMargin

    assertNoDiff(normalizedOutput, expectedOutput)
  }

  test("BytecodeWriter toInst correctness for all instructions") {
    def packABC(op: Int, a: Int, b: Int, c: Int): Int =
      (op & 0x3f) | ((a & 0xff) << 6) | ((b & 0x1ff) << 14) | ((c & 0x1ff) << 23)

    def packABx(op: Int, a: Int, bx: Int): Int =
      (op & 0x3f) | ((a & 0xff) << 6) | ((bx & 0x3ffff) << 14)

    def intToBytes(i: Int): List[Byte] =
      List(
        (i & 0xff).toByte,
        ((i >> 8) & 0xff).toByte,
        ((i >> 16) & 0xff).toByte,
        ((i >> 24) & 0xff).toByte
      )

    val testCases = List(
      MOVE(1, 2) -> packABC(0, 1, 2, 0),
      LOADK(3, 4) -> packABx(1, 3, 4),
      LOADBOOL(5, 1, 0) -> packABC(2, 5, 1, 0),
      LOADNIL(6, 7) -> packABC(3, 6, 7, 0),
      GETUPVAL(8, 9) -> packABC(4, 8, 9, 0),
      GETGLOBAL(10, 11) -> packABx(5, 10, 11),
      GETTABLE(12, 13, 14) -> packABC(6, 12, 13, 14),
      SETGLOBAL(15, 16) -> packABx(7, 15, 16),
      SETUPVAL(17, 18) -> packABC(8, 17, 18, 0),
      SETTABLE(19, 20, 21) -> packABC(9, 19, 20, 21),
      NEWTABLE(22, 23, 24) -> packABC(10, 22, 23, 24),
      ADD(25, 26, 27) -> packABC(12, 25, 26, 27),
      SUB(28, 29, 30) -> packABC(13, 28, 29, 30),
      MUL(31, 32, 33) -> packABC(14, 31, 32, 33),
      DIV(34, 35, 36) -> packABC(15, 34, 35, 36),
      MOD(37, 38, 39) -> packABC(16, 37, 38, 39),
      POW(40, 41, 42) -> packABC(17, 40, 41, 42),
      UNM(43, 44) -> packABC(18, 43, 44, 0),
      NOT(45, 46) -> packABC(19, 45, 46, 0),
      LEN(47, 48) -> packABC(20, 47, 48, 0),
      CONCAT(49, 50, 51) -> packABC(21, 49, 50, 51),
      JMP(52) -> packABx(22, 0, 52),
      EQ(1, 53, 54) -> packABC(23, 1, 53, 54),
      LT(0, 55, 56) -> packABC(24, 0, 55, 56),
      LE(1, 57, 58) -> packABC(25, 1, 57, 58),
      TEST(59, 1) -> packABC(26, 59, 0, 1),
      TESTSET(60, 61, 0) -> packABC(27, 60, 61, 0),
      CALL(62, 2) -> packABC(28, 62, 2, 0),
      RETURN(63) -> packABC(30, 63, 0, 0),
      FORLOOP(1, 100) -> packABx(31, 1, 100),
      FORPREP(2, 200) -> packABx(32, 2, 200),
      SETLIST(3, 50, 1) -> packABC(34, 3, 50, 1),
      CLOSE(4) -> packABC(35, 4, 0, 0),
      CLOSURE(5, 300) -> packABx(36, 5, 300)
    )

    for ((inst, expectedInt) <- testCases) {
      val proto = Proto(List(inst), Map.empty, Map.empty, Map.empty, Nil, 0)
      val bytes = BytecodeWriter.toByteStream(proto).toList
      val obtainedInstBytes = bytes.slice(7, 11)
      val expectedInstBytes = intToBytes(expectedInt)
      assertEquals(obtainedInstBytes, expectedInstBytes, s"Failed for instruction $inst")
    }
  }

  test("Displayer asInstr correctness for all instructions") {
    def packABC(op: Int, a: Int, b: Int, c: Int): Int =
      (op & 0x3f) | ((a & 0xff) << 6) | ((b & 0x1ff) << 14) | ((c & 0x1ff) << 23)

    def packABx(op: Int, a: Int, bx: Int): Int =
      (op & 0x3f) | ((a & 0xff) << 6) | ((bx & 0x3ffff) << 14)

    val testCases = List(
      MOVE(1, 2) -> packABC(0, 1, 2, 0),
      LOADK(3, 4) -> packABx(1, 3, 4),
      LOADBOOL(5, 1, 0) -> packABC(2, 5, 1, 0),
      LOADNIL(6, 7) -> packABC(3, 6, 7, 0),
      GETUPVAL(8, 9) -> packABC(4, 8, 9, 0),
      GETGLOBAL(10, 11) -> packABx(5, 10, 11),
      GETTABLE(12, 13, 14) -> packABC(6, 12, 13, 14),
      SETGLOBAL(15, 16) -> packABx(7, 15, 16),
      SETUPVAL(17, 18) -> packABC(8, 17, 18, 0),
      SETTABLE(19, 20, 21) -> packABC(9, 19, 20, 21),
      NEWTABLE(22, 23, 24) -> packABC(10, 22, 23, 24),
      ADD(25, 26, 27) -> packABC(12, 25, 26, 27),
      SUB(28, 29, 30) -> packABC(13, 28, 29, 30),
      MUL(31, 32, 33) -> packABC(14, 31, 32, 33),
      DIV(34, 35, 36) -> packABC(15, 34, 35, 36),
      MOD(37, 38, 39) -> packABC(16, 37, 38, 39),
      POW(40, 41, 42) -> packABC(17, 40, 41, 42),
      UNM(43, 44) -> packABC(18, 43, 44, 0),
      NOT(45, 46) -> packABC(19, 45, 46, 0),
      LEN(47, 48) -> packABC(20, 47, 48, 0),
      CONCAT(49, 50, 51) -> packABC(21, 49, 50, 51),
      JMP(52) -> packABx(22, 0, 52),
      EQ(1, 53, 54) -> packABC(23, 1, 53, 54),
      LT(0, 55, 56) -> packABC(24, 0, 55, 56),
      LE(1, 57, 58) -> packABC(25, 1, 57, 58),
      TEST(59, 1) -> packABC(26, 59, 0, 1),
      TESTSET(60, 61, 0) -> packABC(27, 60, 61, 0),
      CALL(62, 2) -> packABC(28, 62, 2, 0),
      RETURN(63) -> packABC(30, 63, 0, 0),
      FORLOOP(1, 100) -> packABx(31, 1, 100),
      FORPREP(2, 200) -> packABx(32, 2, 200),
      SETLIST(3, 50, 1) -> packABC(34, 3, 50, 1),
      CLOSE(4) -> packABC(35, 4, 0, 0),
      CLOSURE(5, 300) -> packABx(36, 5, 300)
    )

    for ((expectedInst, packedInt) <- testCases) {
      val decodedInst = compiler.asInstr(packedInt)
      assertEquals(decodedInst, expectedInst, s"Failed decoding instruction int $packedInt")
    }
  }

  test("Displayer constant type decoding correctness") {
    val proto = Proto(
      instructions = Nil,
      constTable = Map(
        LNil -> 0,
        true -> 1,
        false -> 2,
        123.45 -> 3,
        "test_string" -> 4
      ),
      symTable = Map.empty,
      upvalTable = Map.empty,
      fnTable = Nil,
      paramCnt = 0
    )

    val byteList = BytecodeWriter.toByteStream(proto).toList

    val stream = new ByteArrayOutputStream()
    val printStream = new PrintStream(stream)
    Displayer.disp(byteList, printStream)
    printStream.flush()
    val output = stream.toString.replace("\r\n", "\n")

    // We can't guarantee map order in output easily, so we check existence of lines
    assert(output.contains("5 constants"))
    assert(output.contains(": nil: LNil"))
    assert(output.contains(": bool: true"))
    assert(output.contains(": bool: false"))
    assert(output.contains(": number: 123.45"))
    assert(output.contains(": string: test_string"))
  }
