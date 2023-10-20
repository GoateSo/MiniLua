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

val p2 = Parser.program(Tokenizer.tokenize("""
|local xs = {{1,2},{2,4}}
|local y = xs[1*1*1/2][2]
|""".stripMargin))

Parseutil.asString(p2)
