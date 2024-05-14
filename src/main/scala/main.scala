import mng.Compiler

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{CommandLineParser, Try}

@main
def main(compileGroupPath: String): Unit = { 

  val fileSource = Source.fromFile(compileGroupPath)

  var srcDesPath = ArrayBuffer[String]()

  for (line <- fileSource.getLines()){
    srcDesPath += line
  }

  assert(srcDesPath.nonEmpty, "srcdes path should not empty")
  assert( (srcDesPath.length % 2) == 0)

  var i = 0
  while (i < srcDesPath.length) {
    println(s"compile file ${srcDesPath(i)}")
    Compiler.compile(srcFilePath = srcDesPath(i), desFilePath = srcDesPath(i + 1))
    i += 2
  } 

}