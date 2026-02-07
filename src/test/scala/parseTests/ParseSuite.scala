package parseTests

import munit.*
import compiler.*
import compiler.Tokenizer.Token
import compiler.TokenType.*

class ParseSuite extends FunSuite:
  test("tokenize simple") {
    import Tokenizer.Token.*
    var tokens = Tokenizer.tokenize("1 +  1")
    assertEquals(tokens.map(_.cont), List(NUM("1"), SP("+"), NUM("1"), Eof))
  }

  test("tokenize a single number") {
    val tokens = Tokenizer.tokenize("123")
    assertEquals(tokens.map(_.cont), List(NUM("123"), Eof))
  }

  test("tokenize a number with decimal") {
    val tokens = Tokenizer.tokenize("123.45")
    assertEquals(tokens.map(_.cont), List(NUM("123.45"), Eof))
  }

  test("tokenize identifiers") {
    val tokens = Tokenizer.tokenize("a b c")
    assertEquals(tokens.map(_.cont), List(ID("a"), ID("b"), ID("c"), Eof))
  }

  test("tokenize keywords") {
    val tokens = Tokenizer.tokenize("if then else end")
    assertEquals(
      tokens.map(_.cont),
      List(KW("if"), KW("then"), KW("else"), KW("end"), Eof)
    )
  }

  test("tokenize a simple expression") {
    val tokens = Tokenizer.tokenize("a + b")
    assertEquals(tokens.map(_.cont), List(ID("a"), SP("+"), ID("b"), Eof))
  }

  test("tokenize string literal") {
    val tokens = Tokenizer.tokenize("\"hello world\"")
    assertEquals(tokens.map(_.cont), List(STR("\"hello world\""), Eof))
  }

  test("tokenize all operators") {
    val code = "+ - * / % ^ == ~= <= >= < > = ( ) { } [ ] ; : , . .."
    val expected = List(
      SP("+"), SP("-"), SP("*"), SP("/"), SP("%"), SP("^"), SP("=="), SP("~="),
      SP("<="), SP(">="), SP("<"), SP(">"), SP("="), SP("("), SP(")"), SP("{"),
      SP("}"), SP("["), SP("]"), SP(";"), SP(":"), SP(","), SP("."), SP(".."),
      Eof
    )
    val tokens = Tokenizer.tokenize(code)
    assertEquals(tokens.map(_.cont), expected)
  }

  test("parse a simple variable declaration") {
    val tokens = Tokenizer.tokenize("local a = 1")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(List(VarDef("a", LNum(1.0))))
    )
  }

  test("parse a simple function call") {
    val tokens = Tokenizer.tokenize("print(\"hello\")")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(List(FunCall("print", List(LStr("\"hello\"")))))
    )
  }

  test("parse a while loop") {
    val tokens = Tokenizer.tokenize("while true do end")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(List(While(LBool(true), Chunk(Nil))))
    )
  }

  test("parse an if statement") {
    val tokens = Tokenizer.tokenize("if true then end")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(List(If(LBool(true), Chunk(Nil), Nil, None)))
    )
  }

  test("parse a for loop") {
    val tokens = Tokenizer.tokenize("for i = 1, 10 do end")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(For("i", LNum(1.0), LNum(10.0), LNum(1), Chunk(Nil)))
      )
    )
  }

  test("parse a function definition") {
    val tokens = Tokenizer.tokenize("local function foo() end")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(List(FunDef("foo", Nil, Chunk(Nil))))
    )
  }

  test("parse table constructor") {
    val tokens = Tokenizer.tokenize("local a = {}")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(List(VarDef("a", Arr(Nil))))
    )
  }

  test("parse arithmetic precedence") {
    val tokens = Tokenizer.tokenize("local a = 1 + 2 * 3")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef(
            "a",
            BinOp("+", LNum(1.0), BinOp("*", LNum(2.0), LNum(3.0)))
          )
        )
      )
    )
  }

  test("parse logical operator precedence") {
    val tokens = Tokenizer.tokenize("local a = false or true and false")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef(
            "a",
            BinOp(
              "or",
              LBool(false),
              BinOp("and", LBool(true), LBool(false))
            )
          )
        )
      )
    )
  }

  test("parse mixed operators") {
    val tokens = Tokenizer.tokenize("local a = 1 + 2 > 3 and 4 < 5")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef(
            "a",
            BinOp(
              "and",
              BinOp(">", BinOp("+", LNum(1.0), LNum(2.0)), LNum(3.0)),
              BinOp("<", LNum(4.0), LNum(5.0))
            )
          )
        )
      )
    )
  }

  test("parse chained comparisons") {
    val tokens = Tokenizer.tokenize("local a = 1 < 2 and 2 < 3")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef(
            "a",
            BinOp(
              "and",
              BinOp("<", LNum(1.0), LNum(2.0)),
              BinOp("<", LNum(2.0), LNum(3.0))
            )
          )
        )
      )
    )
  }

  test("parse power operator associativity") {
    val tokens = Tokenizer.tokenize("local a = 2 ^ 3 ^ 2")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef(
            "a",
            BinOp("^", LNum(2.0), BinOp("^", LNum(3.0), LNum(2.0)))
          )
        )
      )
    )
  }

  test("parse comprehensive order of operations") {
    val code = "local a = 2^3+5*5/2 < 5 and true or false"
    val tokens = Tokenizer.tokenize(code)
    val tree = Parser.program(tokens)
    import TreeNode.*
    val expected = Chunk(
      List(
        VarDef(
          "a",
          BinOp(
            "or",
            BinOp(
              "and",
              BinOp(
                "<",
                BinOp(
                  "+",
                  BinOp("^", LNum(2.0), LNum(3.0)),
                  BinOp("/", BinOp("*", LNum(5.0), LNum(5.0)), LNum(2.0))
                ),
                LNum(5.0)
              ),
              LBool(true)
            ),
            LBool(false)
          )
        )
      )
    )
    assertEquals(tree, expected)
  }

  test("parse unary operator precedence") {
    val tokens = Tokenizer.tokenize("local a = -2^2") // Should parse as -(2^2)
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef(
            "a",
            UnOp("-", BinOp("^", LNum(2.0), LNum(2.0)))
          )
        )
      )
    )
  }

  test("parse table indexing") {
    val tokens = Tokenizer.tokenize("local a = b[1]")
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef(
            "a",
            TInd(Id("b"), LNum(1.0))
          )
        )
      )
    )
  }

  test("parse multi-block if-elseif-else") {
    val code = """
      if a then
        b
      elseif c then
        d
      elseif e then
        f
      else
        g
      end
    """
    val tokens = Tokenizer.tokenize(code)
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          If(
            Id("a"),
            Chunk(List(Id("b"))),
            List(
              (Id("e"), Chunk(List(Id("f")))),
              (Id("c"), Chunk(List(Id("d"))))
            ),
            Some(Chunk(List(Id("g"))))
          )
        )
      )
    )
  }

  test("parser throws error on missing expression") {
    val tokens = Tokenizer.tokenize("local a =")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws error on unclosed parenthesis") {
    val tokens = Tokenizer.tokenize("local a = (1 + 2")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parse multi-statement chunk with mutation") {
    val code = "local a = 1; a = a + 1"
    val tokens = Tokenizer.tokenize(code)
    val tree = Parser.program(tokens)
    import TreeNode.*
    assertEquals(
      tree,
      Chunk(
        List(
          VarDef("a", LNum(1.0)),
          VarMut("a", BinOp("+", Id("a"), LNum(1.0)))
        )
      )
    )
  }

  test("parser throws on unclosed function arguments") {
    val tokens = Tokenizer.tokenize("my_func(a, b")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on unclosed table constructor") {
    val tokens = Tokenizer.tokenize("local a = {1, 2")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on unclosed table index") {
    val tokens = Tokenizer.tokenize("local val = my_array[1")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on function missing end") {
    val tokens = Tokenizer.tokenize("local function foo()")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on while loop missing do") {
    val tokens = Tokenizer.tokenize("while true end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on for loop missing do") {
    val tokens = Tokenizer.tokenize("for i = 1, 10 end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on for loop with incomplete header") {
    val tokens = Tokenizer.tokenize("for i = 1, do end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on if missing then") {
    val tokens = Tokenizer.tokenize("if true end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on if missing condition") {
    val tokens = Tokenizer.tokenize("if then")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on variable definition missing =") {
    val tokens = Tokenizer.tokenize("local a")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on function definition missing name") {
    val tokens = Tokenizer.tokenize("local function ()")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on function missing closing parenthesis for args") {
    val tokens = Tokenizer.tokenize("local function f(a end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on for loop missing =") {
    val tokens = Tokenizer.tokenize("for i 1, 10 do end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on for loop bad step") {
    val tokens = Tokenizer.tokenize("for i = 1, 10, do end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on for loop missing do after step") {
    val tokens = Tokenizer.tokenize("for i = 1, 10, 1 end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on while missing expression") {
    val tokens = Tokenizer.tokenize("while do end")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on if missing end after else") {
    val tokens = Tokenizer.tokenize("if true then print(1) else print(2)")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on if missing end after then") {
    val tokens = Tokenizer.tokenize("if true then print(1)")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on var assignment missing expression") {
    val tokens = Tokenizer.tokenize("a =")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("parser throws on extra tokens after program") {
    val tokens = Tokenizer.tokenize("local a=1 local")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }

  test("tokenize invalid character") {
    intercept[Exception] {
      Tokenizer.tokenize("@")
    }
  }

  test("parser throws on invalid assignment target") {
    val tokens = Tokenizer.tokenize("f() = 1")
    intercept[Exception] {
      Parser.program(tokens)
    }
  }
