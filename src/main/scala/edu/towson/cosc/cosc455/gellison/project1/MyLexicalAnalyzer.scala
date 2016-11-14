/**
  * Created by Gita on 11/6/2016.
  */
package edu.towson.cosc.cosc455.gellison.project1

class MyLexicalAnalyzer extends LexicalAnalyzer{


  val size = Compiler.source.toList.length
  val sourcelist = Compiler.source.toList
  var tempbucket = new Array[Char](size)
  var tokenBucket = new Array[String](size)

  var currentPos = 0
  val nextPos = currentPos + 1

    def addChar(a: Char): Unit = {

      //if the temp set of chars matches a constant
      //its saved as a token in tokenBucket
      if (lookup(tempbucket.toString)) {
        tokenBucket += tempbucket.toString
        tempbucket = null //set temp bucket to null so we can start this fun process all over again
      }

      //if there is a special char following the text
      //this should close up a token

      if(CONSTANTS.tags.contains(sourcelist(currentPos+1)) ){
        tokenBucket += tempbucket.toString
        tempbucket = null

      }

      else {
        tempbucket :+= a //add char to temp bucket
      }



    }

    //why is it yelling at me about the override
    //checks to see if the token is in the list of constants
    override def lookup (a: String ): Boolean =  {
      return(CONSTANTS.allConstants.contains(a))
    }

    //seperate method that creats a list of all the finalized tokens
    //easier to work with in theory.
    def setTokens(): Unit = {

      for(i <- 0 to size)  {

        val c = getChar()

        if (!CONSTANTS.isLegal(c)) { //error state
          return "Lexical error"
          System.exit(1)
        }
        else if (CONSTANTS.isText(c)) {
          //text processing state
          addChar()
          getChar()

        }
        else if(CONSTANTS.isSpecial(c)){
          //annotation processing state
          addChar()
          getChar()

        }
      }
    }

    def getNextToken(): String = { //pulls the next token in the list
      currentPos = nextPos
      return tokenBucket (nextPos)

    }

     def getChar(): Unit = {  //pulls the next char from the source file which was converted to a list

       if( currentPos < size){
         return sourcelist (currentPos)

       }




    }










}
