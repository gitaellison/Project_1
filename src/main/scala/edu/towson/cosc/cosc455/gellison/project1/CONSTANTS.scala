/**
  * Created by Gita on 11/6/2016.
  */
package edu.towson.cosc.cosc455.gellison.project1

object CONSTANTS {

  //defines Text
  def isText(c: Unit): Boolean = {
    if (illegals.contains(c) || tags.contains(c))
      return false
    return true
  }

  //checks if it is legal lexeme based on list
  def isLegal(c: Unit): Boolean = {
    if (illegals.contains(c))
      return false
    return true
  }

  //checks if it is a special character which would mark a tag or annotation
  def isSpecial(c: Unit): Boolean =
    return tags.contains(c)

  val DOCB : String = 	"\\BEGIN"
  val DOCE : String = 	"\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "]"
  val HEADING : String = "#"
  val PARB  : String = "\\PARB"
  val PARE  : String = "\\PARE"
  val BOLD : String = "**"
  val ITALICS : String = "*"
  val LISTITEM : String = "+"
  val NEWLINE : String = "\\"
  val LINKB : String = "["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = "\\DEF["
  val EQSIGN : String = "="
  val USEB : String = "\\USE["
  //val REQTEXT : String = ""
  //val TEXT : String = ""

  val tags= List('\\','#','*','[','!','+','(',')','=')
  val illegals = List( '~','@','$','%','^','&','-','+','{','}','|','/',';','<','>')
  val allConstants = List(DOCB, DOCE, TITLEB, BRACKETE, HEADING, PARB, PARE, BOLD, ITALICS, LISTITEM, NEWLINE, LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQSIGN, USEB)



}

