import mng.Compiler

import scala.util.{CommandLineParser, Try}

@main
def main(srcFilePath: String, desFilePath: String): Unit = {

  Compiler.compile(srcFilePath, desFilePath)

}