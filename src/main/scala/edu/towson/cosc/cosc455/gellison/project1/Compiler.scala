package edu.towson.cosc.cosc455.gellison.project1



object Compiler {
  var currentToken : String = ""
  var fileContents : String = ""
  var source : String = ""
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))
    currentToken = Scanner.tokenBucket (0) //current token is received from the token bucket
    Scanner.getNextToken() //lexical analyzer is checking for errors
    Parser.gittex() //syntax analyzer is checking for errors
    SemanticAnalyzer.htmlConvert() // if correct, parseTree is produced with the html equivalent displayed
  }


  def readFile(file : String) = {
    source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".mkd")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }

}
