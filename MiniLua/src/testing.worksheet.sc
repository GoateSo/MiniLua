import compiler.CodeGen.flattenOp
import compiler.*
import utils.Parseutil

val e2 = """
|local x = 5 + foo(a,b,{1.2,453.12})
|local lig = bar{1,2,3}
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

val p2 = Parser.program(Tokenizer.tokenize("""|local xs = {{1,2},{2,4}}
                                              |local y = xs[1*1*1/2][2]
                                              |""".stripMargin))

Parseutil.asString(p2)
Parseutil.asString(p)
import TreeNode.*
flattenOp(
  "and",
  BinOp(
    "and",
    BinOp("and", BinOp("+", Id("a"), Id("b")), BinOp("+", Id("c"), Id("d"))),
    BinOp(
      "and",
      BinOp("and", UnOp("-", LNum(1)), Id("f")),
      BinOp("and", Id("g"), Id("h"))
    )
  )
).mkString("\n", "\n", "\n")

val prog = Parser
  .program(
    Tokenizer.tokenize(
      """for i = a,b do local x = 1 local y = 2 end"""
    )
  )

val code = CodeGen
  .processStmt(
    Proto(
      Nil,
      Map.empty,
      Map("a" -> 0, "b" -> 1, "c" -> 2),
      Map.empty,
      List.empty,
      0,
      null
    ),
    prog
  )
  .instructions
  .zipWithIndex
  .map((a, b) => s"$b\t:$a")
  .mkString("\n", "\n", "\n")
