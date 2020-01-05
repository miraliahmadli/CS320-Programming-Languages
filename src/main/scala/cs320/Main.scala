package cs320

import scala.Console.{ RED, RESET }
import scala.reflect.runtime.{ universe => ru }

object Main {
  def main(args: Array[String]): Unit = args.toList match {
    case name :: Nil => runTests(name) match {
      case Some(_) =>
      case None =>
        redMsg(RED + s"Not defined homework: $name" + RESET)
        help
    }
    case _ => help
  }

  def runTests(name: String): Option[Unit] = try {
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val moduleSymbol = runtimeMirror.staticModule("cs320." + name + ".package$")
    val moduleInstance = runtimeMirror.reflectModule(moduleSymbol).instance
    val instMirror = runtimeMirror.reflect(moduleInstance)
    val methodSymbol = instMirror.symbol.info.member(ru.TermName("tests")).asMethod
    instMirror.reflectMethod(methodSymbol)()
    Some(())
  } catch { case e: Throwable => None }

  def help: Unit = {
    redMsg(s"Please use the following format:")
    redMsg()
    redMsg(s"  sbt run hwXX")
  }

  def redMsg(msg: String = ""): Unit = {
    println(RED + msg + RESET)
  }
}
