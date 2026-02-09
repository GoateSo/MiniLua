package parseTests

import munit.*
import compiler.*
import compiler.TreeNode.*
import utils.Parseutil

class ParseutilSuite extends FunSuite:

  test("Parseutil.asString for basic expressions") {
    assertEquals(Parseutil.asString(LNum(1.0)), "1.0")
    assertEquals(Parseutil.asString(LBool(true)), "true")
    assertEquals(Parseutil.asString(LBool(false)), "false")
    assertEquals(Parseutil.asString(LStr("hello")), "hello")
    assertEquals(Parseutil.asString(LNil), "nil")
    assertEquals(Parseutil.asString(Id("foo")), "foo")
  }

  test("Parseutil.asString for operators") {
    assertEquals(Parseutil.asString(BinOp("+", Id("a"), Id("b"))), "(a + b)")
    assertEquals(Parseutil.asString(UnOp("-", Id("a"))), "(-a)")
    assertEquals(Parseutil.asString(TInd(Id("tab"), Id("idx"))), "(tab)[idx]")
  }

  test("Parseutil.asString for statements") {
    assertEquals(Parseutil.asString(VarDef("x", LNum(1.0))), "local x = 1.0")
    assertEquals(Parseutil.asString(VarMut("x", LNum(2.0))), "x = 2.0")
    assertEquals(Parseutil.asString(Break), "break")
    assertEquals(Parseutil.asString(Return(Id("foo"))), "return foo")
  }

  test("Parseutil.asString for control structures") {
    val whileLoop = While(LBool(true), Chunk(List(Id("foo"))))
    assertNoDiff(Parseutil.asString(whileLoop).toString(), """while true do  
      |  foo
      |end""".stripMargin)

    val forLoop = For("i", LNum(1.0), LNum(10.0), LNum(1.0), Chunk(List(Id("foo"))))
    assertNoDiff(Parseutil.asString(forLoop), """for i=1.0, 10.0, 1.0 do  
      |  foo
      |end""".stripMargin.toString())

    val ifStmt = If(
      Id("a"),
      Chunk(List(Id("b"))),
      List((Id("c"), Chunk(List(Id("d"))))),
      Some(Chunk(List(Id("e"))))
    )
    val expectedIf = """if a then  
      |  b
      |elseif c then  
      |  d
      |else  
      |  e
      |end""".stripMargin
    assertNoDiff(Parseutil.asString(ifStmt), expectedIf)
  }

  test("Parseutil.asString for functions and arrays") {
    val funDef = FunDef("my_func", List("a", "b"), Chunk(List(Return(Id("a")))))
    assertNoDiff(Parseutil.asString(funDef), """local function my_func(a, b)   
      |  return a
      |end""".stripMargin)

    val funCall = FunCall("print", List(LStr("hi"), Id("x")))
    assertNoDiff(Parseutil.asString(funCall), "print(hi, x)")

    val array = Arr(List(LNum(1.0), LStr("two")))
    assertNoDiff(Parseutil.asString(array), "{1.0, two}")
  }

  test("Parseutil.asString for complex chunks") {
    val chunk = Chunk(List(
      VarDef("a", LNum(1.0)),
      VarMut("a", BinOp("+", Id("a"), LNum(1.0)))
    ))
    assertNoDiff(Parseutil.asString(chunk), """
      |local a = 1.0
      |a = (a + 1.0)""".stripMargin)
  }

  test("Parseutil.asString for table assignment") {
    val chunk = Chunk(List(
      VarDef("a", Arr(List(LNum(1.0)))),
      TableSet(Id("a"), LNum(1.0), BinOp("+", LNum(2.0), LNum(1.0)))
    ))
    assertNoDiff(Parseutil.asString(chunk), """
      |local a = {1.0}
      |a[1.0] = (2.0 + 1.0)""".stripMargin)
  }
