package codegenTests

import munit.*
import compiler.*
import compiler.Inst.*
import compiler.TreeNode.*

class CodeGenSuite extends FunSuite:
  val emptyProto = Proto(Nil, Map.empty, Map.empty, Map.empty, Nil, 0)

  test("codegen simple constant") {
    val tree = LNum(1.0)
    val result = CodeGen.processExpr(tree, emptyProto, 0)
    val expected = List(
      LOADK(0, -1)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map(1.0 -> -1))
  }

  test("codegen simple addition") {
    val tree = BinOp("+", LNum(1.0), LNum(2.0))
    val result = CodeGen.processExpr(tree, emptyProto, 0)
    val expected = List(
      ADD(0, -1, -2)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map(1.0 -> -1, 2.0 -> -2))
  }

  test("codegen simple local definition") {
    val tree = VarDef("a", LNum(1.0))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      LOADK(0, -1)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map(1.0 -> -1))
    assertEquals(result.symTable, Map("a" -> 0))
  }

  test("codegen simple while loop") {
    val tree = While(LBool(true), Chunk(Nil))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      LOADBOOL(0, 1, 0),
      TEST(0, 0),
      JMP(1),
      JMP(-4)
    )
    assertEquals(result.instructions, expected)
  }

  test("codegen function call") {
    val tree = FunCall("print", List(LStr("hello")))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      GETUPVAL(0, 0),
      LOADK(1, -1),
      CALL(0, 2)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map("hello" -> -1))
    assertEquals(result.upvalTable, Map("print" -> 0))
  }

  test("codegen variable assignment (mutation)") {
    val initialProto = emptyProto.addSymbol("a", 0) // 'a' is already defined at register 0
    val tree = VarMut("a", LNum(10.0))
    val result = CodeGen.processStmt(initialProto, tree)
    val expected = List(
      LOADK(0, -1)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map(10.0 -> -1))
    assertEquals(result.symTable, Map("a" -> 0))
  }

  test("codegen for loop test") {
    val tree = For("i", LNum(1.0), LNum(10.0), LNum(1.0), Chunk(List(Break)))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      LOADK(0, -1),
      LOADK(1, -2),
      LOADK(2, -1),
      FORPREP(0, 1),
      JMP(1),
      FORLOOP(0, -2)
    )
    // Clear constants that have non-deterministic order
    val filteredConstTable = result.constTable.view.mapValues(_.sign).toMap
    assertEquals(result.instructions, expected)
    assertEquals(filteredConstTable, Map(1.0 -> -1, 10.0 -> -1))
  }

  test("codegen if statement") {
    val tree = If(LBool(true), Chunk(Nil), Nil, None)
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      LOADBOOL(0, 1, 0),
      TEST(0, 0),
      JMP(1),
      JMP(0)
    )
    assertEquals(result.instructions, expected)
  }

  test("codegen function definition") {
    val tree = FunDef("foo", Nil, Chunk(Nil))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      CLOSURE(0, 0)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.symTable, Map("foo" -> 0))
    assertEquals(result.fnTable.size, 1)
    val fooProto = result.fnTable.head
    assertEquals(fooProto.instructions, List(RETURN(0)))
    assertEquals(fooProto.paramCnt, 0)
  }

  test("codegen function with params and body") {
    val tree = FunDef("add", List("a", "b"), Chunk(List(Return(BinOp("+", Id("a"), Id("b"))))))
    val result = CodeGen.processStmt(emptyProto, tree)
    assertEquals(result.instructions, List(CLOSURE(0, 0)))
    assertEquals(result.symTable, Map("add" -> 0))
    assertEquals(result.fnTable.size, 1)
    val addProto = result.fnTable.head
    assertEquals(addProto.paramCnt, 2)
    assertEquals(addProto.symTable, Map("a" -> 0, "b" -> 1))
    assertEquals(addProto.instructions, List(ADD(2, 0, 1), RETURN(2), RETURN(2)))
  }

  test("codegen function call in expression") {
    val tree = VarDef("a", BinOp("+", LNum(1.0), FunCall("foo", Nil)))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      GETUPVAL(0, 0),
      CALL(0, 1),
      ADD(0, -1, 0)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map(1.0 -> -1))
    assertEquals(result.symTable, Map("a" -> 0))
    assertEquals(result.upvalTable, Map("foo" -> 0))
  }

  test("codegen array constructor") {
    val tree = VarDef("a", Arr(List(LNum(1.0), LStr("two"))))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      NEWTABLE(0, 2, 0),
      LOADK(1, -1),
      LOADK(2, -2),
      SETLIST(0, 2, 1)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map(1.0 -> -1, "two" -> -2))
    assertEquals(result.symTable, Map("a" -> 0))
  }

  test("codegen unary minus and not") {
    val tree = Chunk(List(
      VarDef("a", UnOp("-", LNum(1.0))),
      VarDef("b", UnOp("not", LBool(true)))
    ))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      UNM(0, -1),
      NOT(1, -2)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map(1.0 -> -1, true -> -2))
    assertEquals(result.symTable, Map("a" -> 0, "b" -> 1))
  }

  test("codegen comparison operators") {
    val ops = List("==", "~=", "<", ">", "<=", ">=")
    val initialProto = emptyProto.addSymbol("a", 0).addSymbol("b", 1)
    for (op <- ops) {
      val tree = BinOp(op, Id("a"), Id("b"))
      val result = CodeGen.processExpr(tree, initialProto, 2)
      val cmpInst = op match {
        case "==" | "~=" => EQ(if (op == "==") 1 else 0, 0, 1)
        case "<" | ">="  => LT(if (op == "<") 1 else 0, 0, 1)
        case "<=" | ">" => LE(if (op == "<=") 1 else 0, 0, 1)
      }
      val expected = List(
        cmpInst,
        JMP(1),
        LOADBOOL(2, 0, 1),
        LOADBOOL(2, 1, 0)
      )
      assertEquals(result.instructions, expected, s"Failed on operator $op")
    }
  }

  test("codegen string concatenation") {
    val tree = BinOp("..", LStr("a"), LStr("b"))
    val result = CodeGen.processExpr(tree, emptyProto, 0)
    val expected = List(
      LOADK(0, -1),
      LOADK(1, -2),
      CONCAT(0, 0, 1)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.constTable, Map("a" -> -1, "b" -> -2))
  }

  test("codegen chained 'and' expression") {
    val tree = BinOp("and", Id("a"), BinOp("and", Id("b"), Id("c")))
    val initialProto = emptyProto.addSymbol("a", 0).addSymbol("b", 1).addSymbol("c", 2)
    val result = CodeGen.processExpr(tree, initialProto, 3)
    val expected = List(
      TESTSET(3, 0, 0),
      JMP(3),
      TESTSET(3, 1, 0),
      JMP(1),
      MOVE(3, 2)
    )
    assertEquals(result.instructions, expected)
  }

  test("codegen chained 'or' expression") {
    val tree = BinOp("or", Id("a"), BinOp("or", Id("b"), Id("c")))
    val initialProto = emptyProto.addSymbol("a", 0).addSymbol("b", 1).addSymbol("c", 2)
    val result = CodeGen.processExpr(tree, initialProto, 3)
    val expected = List(
      TESTSET(3, 0, 1),
      JMP(3),
      TESTSET(3, 1, 1),
      JMP(1),
      MOVE(3, 2)
    )
    assertEquals(result.instructions, expected)
  }

  test("codegen return local variable") {
    val tree = Return(Id("a"))
    val initialProto = emptyProto.addSymbol("a", 0)
    val result = CodeGen.processStmt(initialProto, tree)
    assertEquals(result.instructions, List(RETURN(0)))
  }

  test("codegen return upvalue") {
    val tree = Return(Id("b"))
    val result = CodeGen.processStmt(emptyProto, tree)
    val expected = List(
      GETUPVAL(0, 0),
      RETURN(0)
    )
    assertEquals(result.instructions, expected)
    assertEquals(result.upvalTable, Map("b" -> 0))
  }

  test("codegen invalid unary operator") {
    val tree = UnOp("foo", LNum(1.0))
    intercept[Exception] {
      CodeGen.processExpr(tree, emptyProto, 0)
    }
  }

  test("codegen invalid binary operator") {
    val tree = BinOp("bar", LNum(1.0), LNum(1.0))
    intercept[Exception] {
      CodeGen.processExpr(tree, emptyProto, 0)
    }
  }

  test("codegen empty table") {
    val tree = Arr(Nil)
    val result = CodeGen.processExpr(tree, emptyProto, 0)
    val expected = List(
      NEWTABLE(0, 0, 0)
    )
    assertEquals(result.instructions, expected)
  }

  test("codegen multilevel upvalues") {
    // function outer()
    //   local x = 10
    //   function mid()
    //     function inner()
    //       return x
    //     end
    //   end
    // end
    val outerProto = emptyProto.addSymbol("x", 0)
    val midProto = Proto(Nil, Map.empty, Map.empty, Map.empty, Nil, 0, outerProto)
    val innerTree = FunDef("inner", Nil, Chunk(List(Return(Id("x")))))
    
    // We process the inner function definition within the context of 'mid'
    val result = CodeGen.processStmt(midProto, innerTree)
    
    // inner function proto is at index 0 of mid's fnTable
    val innerProto = result.fnTable.head
    
    // Inner function should use GETUPVAL to access x
    // x is upval 0 in inner
    assertEquals(innerProto.instructions, List(GETUPVAL(0, 0), RETURN(0), RETURN(0)))
    assertEquals(innerProto.upvalTable, Map("x" -> 0))
    
    // Mid function should have x as upval too (bridging)
    assertEquals(result.upvalTable, Map("x" -> 0))
  }

  test("codegen complex logical expression") {
    // (1+2) and 3 or 4
    val tree = BinOp("or", 
      BinOp("and", 
        BinOp("+", LNum(1.0), LNum(2.0)), 
        LNum(3.0)
      ), 
      LNum(4.0)
    )
    
    val result = CodeGen.processExpr(tree, emptyProto, 0)
    
    val expected = List(
      // (1+2) -> Optimized to ADD R0, K1, K2
      ADD(0, -1, -2), 
      // AND test
      TEST(0, 0),
      JMP(1),
      // Load 3
      LOADK(0, -3),
      // OR test
      TEST(0, 1),
      JMP(1),
      // Load 4
      LOADK(0, -4)
    )
    
    // Ignore constant table values, just check instructions
    assertEquals(result.instructions, expected)
  }

  test("proto toString") {
    val input = Chunk(List(VarDef("x",LNum(1.0)), VarDef("z",BinOp("and",Id("x"),LNum(2.0)))))
    val expected = """[proto]
                    |Parameter Count: 0
                    |Instructions:
                    |0	:LOADK(0,-1)
                    |1	:TESTSET(1,0,0)
                    |2	:JMP(1)
                    |3	:LOADK(1,-2)
                    |Constants:
                    |-2	2.0
                    |-1	1.0
                    |Symbols:
                    |0	x
                    |1	z
                    |Upvalues:
                    |
                    |Functions:
                    |""".stripMargin
    val result = CodeGen.processStmt(emptyProto, input)

    assertNoDiff(result.toString, expected)
  }

  test("invalid statement / expr") {
    val badExpr = Chunk(List(VarDef("x",LNum(1.0)), VarDef("z",BinOp("and",Id("x"),LNum(2.0)))))
    val badStmt = BinOp("+", LNum(1.0), LNum(2.0))
    
    intercept[Exception] {
      CodeGen.processExpr(badExpr, emptyProto, 0)
    }
    intercept[Exception] {
      CodeGen.processStmt(emptyProto, badStmt)
    }
  }

  test("invalid statement / expr") {
    val badExpr = Chunk(List(VarDef("x",LNum(1.0)), VarDef("z",BinOp("and",Id("x"),LNum(2.0)))))
    val badStmt = BinOp("+", LNum(1.0), LNum(2.0))
    
    intercept[Exception] {
      CodeGen.processExpr(badExpr, emptyProto, 0)
    }
    intercept[Exception] {
      CodeGen.processStmt(emptyProto, badStmt)
    }
  }