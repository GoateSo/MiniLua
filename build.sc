import mill._
import mill.scalalib._

object MiniLua extends ScalaModule {

  def scalaVersion = "3.3.0"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::pprint:0.7.0",
    ivy"com.lihaoyi::os-lib:0.9.1",
    ivy"com.lihaoyi::sourcecode:0.3.0",
    ivy"com.lihaoyi::fastparse:3.0.2"
  )

  // object test extends ScalaTests with TestModule.Munit {
  //   def ivyDeps = Agg(
  //     ivy"org.scalameta::munit::0.7.29"
  //   )
  // }
}
