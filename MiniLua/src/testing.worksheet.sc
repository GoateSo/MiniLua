import java.nio.file.Files
import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.File
import compiler.Displayer
import compiler.CodeGen.flattenOp
import compiler.*
import utils.Parseutil

val e2 = """
|local x = 5 + foo(a,b,{1.2,453.12})
|local lig = bar{1,2,3}
|local xs = {1,2,3,4}
|local y = xs[({1,2,3})[1]]
|local logma = ooga"aaaaa"
|local y = 1+2+a
|local z = a and b or 3
|local function foo(a,b,c)
|  local x = 1 .. "aaa"
|  local y = 1+2
|  return x/y + 2
|end 
|for i = 1, 10 do
|  print(i)
|end 
|while true do
|   print("a")
|end
|if true then 
| print("a") 
|elseif false then 
| print("b") 
|else 
| print("c") 
|end
|logma = 1
|foo()
""".strip().stripMargin
val p = Parser.program(Tokenizer.tokenize(e2))
println(Parseutil.asString(p))

val proto = CodeGen.processStmt(
  Proto(
    Nil,
    Map(),
    Map(),
    Map(),
    List(),
    0,
    null
  ),
  p
)
println(proto)

val p2 = Parser.program(Tokenizer.tokenize("""|local xs = {{1,2},{2,4}}
                                              |local y = xs[1*1*1/2][2]
                                              |""".stripMargin))

Parseutil.asString(p2)
Parseutil.asString(p)
import TreeNode.*

val prog = Parser
  .program(
    Tokenizer.tokenize(
      """local x = 1 local function f() local function g() local function h() return x end end end f()"""
    )
  )
val code = CodeGen
  .processStmt(
    Proto(
      Nil,
      Map(),
      Map(),
      Map(),
      List(),
      0,
      null
    ),
    prog
  )

// .instructions
// .zipWithIndex
// .map((a, b) => s"$b\t:$a")
// .mkString("\n", "\n", "\n")

BytecodeWriter.writeToFile(proto, "test.luaout")
// convert test.luaout into a ByteArraInputStream
val byts = Files.readAllBytes(File("test.luaout").toPath()).toList
Displayer.disp(byts)
