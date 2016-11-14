/**
  * Created by Gita on 10/26/2016.
  */

package edu.towson.cosc.cosc455.gellison.project1

trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup() : Boolean
}