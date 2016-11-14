
/**
  * Created by Gita on 11/6/2016.
  */
package edu.towson.cosc.cosc455.gellison.project1

import java.awt.Desktop
import java.io._

class MySemanticAnalyzer {
  var converTree = new scala.collection.mutable.Stack[String]
  var parseTree = Compiler.Parser.parseTree.reverse
  var nextToken = parseTree.pop()
  var flag = !parseTree.isEmpty
  var a = ""
  var temp = ""

  def htmlConvert() = {
    while (flag) //while the original parse tree isnt empty
      if (nextToken.equalsIgnoreCase(CONSTANTS.DOCB)) { // \BEGIN
        converTree.push("<html>")
        nextToken = parseTree.pop()
      }

    if (nextToken.equalsIgnoreCase(CONSTANTS.DEFB)) { // \DEF[
      parseTree.pop() //name
      parseTree.pop() //=
      a = nextToken //bob
      nextToken = parseTree.pop()
    }

    if (isText(nextToken)) { //broken method that checks to make sure token is text
      converTree.push(nextToken)
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.TITLEB)) { // \TITLE[
      converTree.push("<head>")
      converTree.push("<title")
      converTree.push(parseTree.pop())
      converTree.push("</title")
      converTree.push("</head>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.HEADING)) { // #
      converTree.push("<h1>")
      converTree.push(parseTree.pop())
      converTree.push("</h1>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.PARB)) { // \PARB
      converTree.push("<p>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.PARE)) { // \PARE
      converTree.push("</p>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.BOLD)) // * *
      converTree.push("<b>")
      converTree.push(parseTree.pop())
      converTree.push("</b>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.ITALICS)) { //*
      converTree.push("<i>")
      converTree.push(parseTree.pop())
      converTree.push("</i>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) { //+
      converTree.push("<li>")
      converTree.push(parseTree.pop())
      converTree.push("</li>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) { // \\
      converTree.push("<br>")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.LINKB)) { // [
      temp = parseTree.pop() //temp = title
      parseTree.pop() // ']'
      nextToken = parseTree.pop()
      if (nextToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) // ' (  '
        nextToken = parseTree.pop() //the actual address
      converTree.push("<a href= \"")
      converTree.push(nextToken)
      converTree.push("\">")
      converTree.push(temp)
      converTree.push("</a>")

      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {//![
      converTree.push("<img src=\"")
      temp = parseTree.pop() //name
      parseTree.pop() // ]
      parseTree.pop() // (
      converTree.push(parseTree.pop()) //link
      converTree.push("\" alt=")
      converTree.push(temp) //name
      converTree.push("\">")
      nextToken = parseTree.pop()
    }

    if (nextToken.equalsIgnoreCase(CONSTANTS.USEB)) { //\USE
      converTree.push(a) //defined in DEFB
      parseTree.pop() // ]
      nextToken = parseTree.pop()
    }

    converTree.reverse //get it back in the right order



  def isText(a: String): Boolean = {
    Compiler.currentToken contains CONSTANTS.illegals
    false
    Compiler.currentToken contains CONSTANTS.tags
    false

    true

  }

  val file = new File("output.html")
  print()
  openHTMLFileInBrowser("output.html")


  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }

  def print(): Unit ={
    val pw = new PrintWriter(file))
    pw.write(converTree.reverse.toString())
    pw.close
  }
}
