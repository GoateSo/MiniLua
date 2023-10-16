import compiler.Tokenizer
val prog = """|local x = 1
              |local yaaa = 2.+2+2-21
              |local z = x + yaaa
              |local s="aaa sa\\\n\"aa" """.stripMargin.replace("\r", "")
val toks = Tokenizer.tokenize(prog)

toks.foreach(println(_))
