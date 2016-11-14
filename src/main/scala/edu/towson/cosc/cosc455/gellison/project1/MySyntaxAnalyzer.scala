/**
  * Created by Gita on 11/6/2016.
  */

package edu.towson.cosc.cosc455.gellison.project1

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var syntax = List()
  // Flag for errors and helper methods
  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError : Boolean = errorFound
  var parseTree = new scala.collection.mutable.Stack[String] //to be used in semantics

  //definition of the whole language
  override def gittex(): Unit = {
    resetError()
    if(!errorFound) docb()
    if(!errorFound) variableDefine()
    if(!errorFound) title()
    if(!errorFound) body()
    if(!errorFound) doce()

    else {
      println("Error")
      System.exit(1)
    }
  }


  //definition for paragraph
  override def paragraph(): Unit = {
    if (!errorFound)parb()
    if (!errorFound)variableDefine()
    if (!errorFound)innerText()
    if (!errorFound)pare()
  }

  //definition for inneritem
  override def innerItem(): Unit = {
    if (!errorFound) {
      variableUse()
      if (!errorFound) innerItem()
    }
    else if (!errorFound){
      bold()
      if (!errorFound) innerItem()
    }
    else if (!errorFound){
      italics()
      if (!errorFound) innerItem()
    }
    else if(isText()){
      parseTree.push(Compiler.currentToken)
      if (!errorFound) innerItem()
    }
  }

  //definition for innerText
  override def innerText(): Unit = {
    if (!errorFound) {
      variableUse()
      if (!errorFound) innerText()
    }
    else if (!errorFound) {
      bold()
      if (!errorFound) innerText()
      }
    else if
      (!errorFound) {
      italics()
      if (!errorFound) innerText()
      }
    else if (!errorFound) {
      listItem()
      if (!errorFound) innerText()
    }
    else if (!errorFound) {
      image()
      if (!errorFound) innerText()
    }
    else if (!errorFound) {
      link()
      if (!errorFound) innerText()
    }
    else if (isText()) {
      parseTree.push(Compiler.currentToken)
      if (!errorFound) innerText()
    }
  }


  //this is wrong but, it's supposed to make sure the token doesn't contain symbols that aren't part of the gittex text
  def isText(): Boolean = {
   Compiler.currentToken contains CONSTANTS.illegals
    false
    Compiler.currentToken contains CONSTANTS.tags
    false

    true

  }

  //defines link
  override def link(): Unit = {
    if (!errorFound) linkb()
    if (isText())
      parseTree.push(Compiler.currentToken)
    if (!errorFound) brackete()
    if (!errorFound) addressb()
    if (isText())
      parseTree.push(Compiler.currentToken)
    if (!errorFound) addresse()
  }

  //defines italics
  def italics(): Unit = {
    if (!errorFound) italics1()
    if (isText())
      parseTree.push(Compiler.currentToken)
    if (!errorFound) italics1()
  }

  //definition for body
  override def body(): Unit = {
    if (!errorFound) {
      innerText()
      if (!errorFound) body()
    }

    else if (!errorFound){
      paragraph()
      if(!errorFound) body()
    }

    else if (!errorFound) {
      newline()
      if(!errorFound) body()

    }
  }

  //definition for bold
  override def bold(): Unit = {
    if (!errorFound) bold1()
    if(isText())
      parseTree.push(Compiler.currentToken)
    if (!errorFound) bold1()
  }

  //definition for newline
  override def newline(): Unit = {
    if(!errorFound) newline1()
  }

  //definition for title
  override def title(): Unit = {
    if(!errorFound) titleb()
    if(isText())
      parseTree.push(Compiler.currentToken)
    if(!errorFound) brackete()
  }

  //definition for variabledefine
  override def variableDefine(): Unit = {
    if(!errorFound) defb()
    if(isText())
      parseTree.push(Compiler.currentToken)
    if(!errorFound) eqsign()
    if(isText())
      parseTree.push(Compiler.currentToken)
    if(!errorFound) brackete()
    if(!errorFound) variableDefine()
  }

  //definition for image
  override def image(): Unit = {
    if(!errorFound) imageb()
    if(isText())
      parseTree.push(Compiler.currentToken)
    if(!errorFound) brackete()
    if(!errorFound) addressb()
    if(isText())
      parseTree.push(Compiler.currentToken)
    if(!errorFound) addresse()

  }

  //definition for variable use
  override def variableUse(): Unit ={
    if(!errorFound)  useb()
    if(isText())
      parseTree.push(Compiler.currentToken)
    if(!errorFound) brackete()

  }

  //definition for heading
  override def heading(): Unit = {

    if(!errorFound) heading1()
    if(isText())
      parseTree.push(Compiler.currentToken)
  }

  //definition for listitem
  override def listItem(): Unit = {
    if(!errorFound) listitemb()
    if(!errorFound) innerItem()
    if(!errorFound) listItem()

  }

  //defition for doce
  def doce(): Unit =
  {
    if((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.DOCE+" was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }

  }

  //definition for italics
  def italics1(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.ITALICS + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for listitem beginning
  def listitemb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.LISTITEM + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for heading
  def heading1(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.HEADING + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for image beginning
  def imageb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.IMAGEB + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for useb
  def useb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.USEB + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for equal sign
  def eqsign(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.EQSIGN + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for title beginning
  def titleb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.TITLEB + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for newline
  def newline1(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.NEWLINE + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for bold
  def bold1(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.BOLD + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for address beginning
  def addressb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB))) {
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.ADDRESSB + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for address end
  def addresse(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.ADDRESSE + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for link beginning
  def linkb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.LINKB + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for paragraph end
  def pare(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARE))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.PARE + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for paragraph beginning
  def parb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARB))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.PARB + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for variable definition
  def defb(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.DEFB + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for end bracket
  def brackete(): Unit = {
    if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))) {
      parseTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR -" + CONSTANTS.BRACKETE + " was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  //definition for document beginning
  def docb(): Unit =
    {
      if((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB))) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
      println("SYNTAX ERROR -" + CONSTANTS.DOCB+" was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }

  }


}
