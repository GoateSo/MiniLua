package utils
import compiler.Inst
import compiler.Inst.*
object GenUtils:
  val ops = Map[String, (Int, Int, Int) => Inst](
    "+"  -> ADD.apply,
    "-"  -> SUB.apply,
    "*"  -> MUL.apply,
    "/"  -> DIV.apply,
    "%"  -> MOD.apply,
    "^"  -> POW.apply,
    ".." -> CONCAT.apply
  )

  val compares = Map(
    "~=" -> EQ.apply,
    "==" -> EQ.apply,
    "<"  -> LT.apply,
    ">"  -> LT.apply,
    "<=" -> LE.apply,
    ">=" -> LE.apply
  )
