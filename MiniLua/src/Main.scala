import compiler.* 
import mainargs.{main, arg, Parser, Flag}
import java.nio.file.Files
import java.io.File

object Main{
  @main
  def run(
    @arg(short = 'i', doc = "input file")
    input: String,
    @arg(short = 'o', doc = "output file")
    output: String,
    @arg(short = 'd', doc = "display contents of output into stdout")
    dispOut: Flag
  ) = {
    val inputCode = Files.readString(File(input).toPath())
    val ast = compiler.Parser.program(Tokenizer.tokenize(inputCode))
    val proto = CodeGen.processStmt(Proto(Nil, Map(),Map(),Map(),List(),0,null), ast)

    BytecodeWriter.writeToFile(proto, output)
    if dispOut.value then
      Displayer.disp(Files.readAllBytes(File(output).toPath()).toList)
  }
  def main(args: Array[String]): Unit = Parser(this).runOrExit(args)
}
