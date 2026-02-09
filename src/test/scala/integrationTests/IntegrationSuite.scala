package integrationTests

import munit.*
import compiler.*
import compiler.Tokenizer.*
import compiler.TokenType.*
import compiler.TreeNode.*
import compiler.Inst.*
import java.io.ByteArrayOutputStream
import java.io.PrintStream

class IntegrationSuite extends FunSuite:

  test("Full compiler pipeline integration test") {
    val source = """
      local a = 1
      local function foo(x)
        return x + a
      end
      local res = foo(2)
    """

    // 1. Tokenizer
    val tokens = Tokenizer.tokenize(source)
    // Verify a few key tokens to ensure tokenization worked
    assert(tokens.exists(t => t.cont == KW("local")))
    assert(tokens.exists(t => t.cont == ID("a")))
    assert(tokens.exists(t => t.cont == NUM("1")))
    assert(tokens.exists(t => t.cont == KW("function")))
    assert(tokens.last.cont == Eof)

    // 2. Parser
    val ast = Parser.program(tokens)
    // Check structure: Chunk with 3 statements
    ast match
      case Chunk(stmts) =>
        assertEquals(stmts.length, 3)
        assert(stmts(0).isInstanceOf[VarDef]) // local a = 1
        assert(stmts(1).isInstanceOf[FunDef]) // local function foo...
        assert(stmts(2).isInstanceOf[VarDef]) // local res = foo(2)
      case _ => fail("AST root should be a Chunk")

    // 3. CodeGen
    val emptyProto = Proto(Nil, Map.empty, Map.empty, Map.empty, Nil, 0)
    val proto = CodeGen.processStmt(emptyProto, ast)
    
    // Check Proto properties
    // main chunk should have 3 locals: a, foo, res
    assertEquals(proto.symTable.size, 3)
    assert(proto.symTable.contains("a"))
    assert(proto.symTable.contains("foo"))
    assert(proto.symTable.contains("res"))
    
    // Should have 1 nested function (foo)
    assertEquals(proto.fnTable.size, 1)
    val fooProto = proto.fnTable.head
    // foo takes 1 param (x)
    assertEquals(fooProto.paramCnt, 1)
    // foo uses 'a' as upvalue (since it's local in parent)
    assert(fooProto.upvalTable.contains("a"))

    // Check instructions in main chunk
    // Should have LOADK (a=1), CLOSURE (foo), various MOVE/GETUPVAL/CALL for the call
    assert(proto.instructions.exists(_.isInstanceOf[LOADK]))
    assert(proto.instructions.exists(_.isInstanceOf[CLOSURE]))
    assert(proto.instructions.exists(_.isInstanceOf[CALL]))

    // 4. BytecodeWriter
    val bytesQueue = BytecodeWriter.toByteStream(proto)
    val bytes = bytesQueue.toList
    assert(bytes.nonEmpty)
    
    // Basic header checks (upvals=0, params=0 for main chunk)
    assertEquals(bytes(0), 0.toByte)
    assertEquals(bytes(1), 0.toByte)

    // 5. Displayer (Round trip check of sorts)
    val stream = new ByteArrayOutputStream()
    val printStream = new PrintStream(stream)
    Displayer.disp(bytes, printStream)
    printStream.flush()
    val decompiled = stream.toString
    
    // Verify decompiled output contains key elements
    assert(decompiled.contains("0 upvalues, 0 param")) // Main chunk header
    assert(decompiled.contains("LOADK"))
    assert(decompiled.contains("CLOSURE"))
    assert(decompiled.contains("1 functions")) // Main chunk lists 1 function
    assert(decompiled.contains("1 upvalues, 1 param")) // foo function header
    assert(decompiled.contains("GETUPVAL")) // foo accesses a
    assert(decompiled.contains("ADD")) // foo does addition
    assert(decompiled.contains("RETURN"))
  }

  test("Advanced compiler pipeline integration test") {
    val source = """
      local s = "a" .. "b"
      local t = { 10, 20, 30 }
      local idx = 1
      local val = t[idx]
      
      while idx < 10 do
        if idx == 5 then
          break
        elseif idx == 3 then
          t[idx] = 999
        else
          t[idx] = idx * -2^2/2%2-2+2
        end
        idx = idx + 1
      end
    """

    // 1. Tokenizer
    val tokens = Tokenizer.tokenize(source)
    assert(tokens.exists(t => t.cont == KW("while")))
    assert(tokens.exists(t => t.cont == KW("break")))
    assert(tokens.exists(t => t.cont == KW("elseif")))
    assert(tokens.exists(t => t.cont == KW("else")))
    assert(tokens.exists(t => t.cont == SP("{")))
    assert(tokens.exists(t => t.cont == SP("}")))

    // 2. Parser
    val ast = Parser.program(tokens)
    ast match
      case Chunk(stmts) =>
        // Verify we have VarDefs and the While loop
        assert(stmts.exists(_.isInstanceOf[VarDef]))
        val loop = stmts.find(_.isInstanceOf[While]).get.asInstanceOf[While]
        loop.body match
          case Chunk(bodyStmts) =>
            // Body should contain If and VarMut
            assert(bodyStmts.exists(_.isInstanceOf[If]))
            assert(bodyStmts.exists(_.isInstanceOf[VarMut]))
            // Verify If structure
            val ifStmt = bodyStmts.find(_.isInstanceOf[If]).get.asInstanceOf[If]
            assert(ifStmt.body match { case Chunk(s) => s.contains(Break) case _ => false }) // if ... then break
            assert(ifStmt.elifs.nonEmpty) // elseif ...
            assert(ifStmt.elseBody.isDefined) // else ...
          case _ => fail("While body should be a Chunk")
      case _ => fail("AST root should be a Chunk")

    // 3. CodeGen
    val emptyProto = Proto(Nil, Map.empty, Map.empty, Map.empty, Nil, 0)
    val proto = CodeGen.processStmt(emptyProto, ast)

    // Check symbol table
    assert(proto.symTable.contains("t"))
    assert(proto.symTable.contains("idx"))
    assert(proto.symTable.contains("val"))

    // Check instructions
    val insts = proto.instructions
    assert(insts.exists(_.isInstanceOf[NEWTABLE])) // Table creation
    assert(insts.exists(_.isInstanceOf[SETLIST]))  // Initializing list
    assert(insts.exists(_.isInstanceOf[GETTABLE])) // Reading t[idx]
    assert(insts.exists(_.isInstanceOf[SETTABLE])) // Writing t[idx] = ...
    assert(insts.exists(_.isInstanceOf[JMP]))      // Control flow
    assert(insts.exists(_.isInstanceOf[LT]))       // idx < 10
    assert(insts.exists(_.isInstanceOf[EQ]))       // idx == 5

    // 4. BytecodeWriter
    val bytesQueue = BytecodeWriter.toByteStream(proto)
    val bytes = bytesQueue.toList
    assert(bytes.nonEmpty)

    // 5. Displayer
    val stream = new ByteArrayOutputStream()
    val printStream = new PrintStream(stream)
    Displayer.disp(bytes, printStream)
    printStream.flush()
    val decompiled = stream.toString

    assert(decompiled.contains("NEWTABLE"))
    assert(decompiled.contains("SETLIST"))
    assert(decompiled.contains("GETTABLE"))
    assert(decompiled.contains("SETTABLE"))
    assert(decompiled.contains("JMP"))
    assert(decompiled.contains("EQ"))
  }

  test("Function call with table argument integration test") {
    val source = "foo {1, 2, 3}"

    // 1. Tokenizer
    val tokens = Tokenizer.tokenize(source)
    assert(tokens.exists(t => t.cont == ID("foo")))
    assert(tokens.exists(t => t.cont == SP("{")))
    assert(tokens.exists(t => t.cont == SP("}")))

    // 2. Parser
    val ast = Parser.program(tokens)
    ast match
      case Chunk(stmts) =>
        assert(stmts.nonEmpty)
        val call = stmts.head
        assert(call.isInstanceOf[FunCall])
        val fc = call.asInstanceOf[FunCall]
        assertEquals(fc.name, "foo")
        assertEquals(fc.args.length, 1)
        assert(fc.args.head.isInstanceOf[Arr])
      case _ => fail("AST root should be a Chunk")

    // 3. CodeGen
    val emptyProto = Proto(Nil, Map.empty, Map.empty, Map.empty, Nil, 0)
    // Add 'foo' as a global/upvalue so we can call it
    val proto = CodeGen.processStmt(emptyProto, ast)

    // Check instructions
    val insts = proto.instructions
    assert(insts.exists(_.isInstanceOf[GETUPVAL])) // Load 'foo'
    assert(insts.exists(_.isInstanceOf[NEWTABLE])) // Create table
    assert(insts.exists(_.isInstanceOf[SETLIST]))  // Populate table
    assert(insts.exists(_.isInstanceOf[CALL]))     // Call foo
  }

  test("Nil, string function call, and variable mutation integration test") {
    val source = """
      local x = nil
      print "hello world"
      x = 100
    """

    // 1. Tokenizer
    val tokens = Tokenizer.tokenize(source)
    assert(tokens.exists(t => t.cont == KW("nil")))
    assert(tokens.exists(t => t.cont == STR("\"hello world\"")))
    assert(tokens.exists(t => t.cont == SP("=")))

    // 2. Parser
    val ast = Parser.program(tokens)
    ast match
      case Chunk(stmts) =>
        assertEquals(stmts.length, 3)
        assert(stmts(0).isInstanceOf[VarDef]) // local x = nil
        assertEquals(stmts(0).asInstanceOf[VarDef].value, LNil)
        
        assert(stmts(1).isInstanceOf[FunCall]) // print "hello world"
        val fc = stmts(1).asInstanceOf[FunCall]
        assertEquals(fc.name, "print")
        assertEquals(fc.args, List(LStr("\"hello world\"")))
        
        assert(stmts(2).isInstanceOf[VarMut]) // x = 100
        assertEquals(stmts(2).asInstanceOf[VarMut].name, "x")
      case _ => fail("AST root should be a Chunk")

    // 3. CodeGen
    val emptyProto = Proto(Nil, Map.empty, Map.empty, Map.empty, Nil, 0)
    val proto = CodeGen.processStmt(emptyProto, ast)

    // Check instructions
    val insts = proto.instructions
    assert(insts.exists(_.isInstanceOf[LOADNIL]))  // local x = nil
    assert(insts.exists(_.isInstanceOf[GETUPVAL])) // load 'print'
    assert(insts.exists(_.isInstanceOf[CALL]))     // call print
    assert(insts.exists(i => i.isInstanceOf[LOADK] && proto.constTable.contains(100.0))) // x = 100
  }

  test("Length operator integration test") {
    val source = """
      local t = { 1, 2, 3 }
      local len = #t
    """
    val tokens = Tokenizer.tokenize(source)
    val ast = Parser.program(tokens)
    val proto = CodeGen.processStmt(Proto(Nil, Map.empty, Map.empty, Map.empty, Nil, 0), ast)
    assert(proto.instructions.exists(_.isInstanceOf[LEN]))
  }

  
