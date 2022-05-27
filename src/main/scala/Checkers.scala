import scala.io.StdIn.readLine

object Checkers{
	
	var board :Array[Array[String]] = Array(
		Array("- ", ".O", "- ", ".O", "- ", ".O", "- ", ".O"),
		Array(".O", "- ", ".O", "- ", ".O", "- ", ".O", "- "),
		Array("- ", ".O", "- ", ".O", "- ", ".O", "- ", ".O"),
		Array(". ", "- ", ". ", "- ", ". ", "- ", ". ", "- "),
		Array("- ", ". ", "- ", ". ", "- ", ". ", "- ", ". "),
		Array(".o", "- ", ".o", "- ", ".o", "- ", ".o", "- "),
		Array("- ", ".o", "- ", ".o", "- ", ".o", "- ", ".o"),
		Array(".o", "- ", ".o", "- ", ".o", "- ", ".o", "- ") )

	def getDrawer: Array[Array[String]] => Unit = CheckersDrawer
	def getController: (Array[Array[String]], String, Boolean) => Boolean = CheckersController
	def getBoard: Array[Array[String]] = board

	def CheckersDrawer(board: Array[Array[String]]): Unit = {
		/* Prints the whole board with its content in a square format. */
		def printBoard(b: Array[Array[String]]): Unit = {
			// Reset font color & background color
			val RESET = "\u001b[0m" // Text Reset
			// Bold font color
			val BLACK_BOLD = "\u001b[1;30m"
			val WHITE_BOLD_BRIGHT = "\u001b[1;97m"
			// Background colors
			val WHITE_BACKGROUND = "\u001b[47m"
			val BLACK_BACKGROUND_BRIGHT = "\u001b[0;100m"
			//setting colors
			val bg1 = WHITE_BACKGROUND //block color 1
			val bg2 = BLACK_BACKGROUND_BRIGHT //block color 2
			val p1 = WHITE_BOLD_BRIGHT //player 1's pieces color
			val p2 = BLACK_BOLD //player 2's pieces color
			val reset = RESET
			//print the Checkers board
			println("\nThe Checkers Board:")
			println("\t\tBlack Player (2)")
			print("    ")
			//print A to H above the board
			List("A  ", "B  ", "C  ", "D  ", "E  ", "F  ", "G  ", "H  ") foreach print
			println()
			for(i<- b.indices){
				print(s"${8-i}  " + bg1);  //print 1 to 8 on the left side
				for(j<- b.indices){
					print(s"${if(b(i)(j)(0) == '-') bg1 else bg2}" +
						s" ${if(b(i)(j)(1).isLower) p1 else p2}${b(i)(j)(1)} ")
				}
				println(s"$reset  ${8-i}"); //print 1 to 8 on the right side
			}
			print("    ")
			//print A to H below the board
			List("A  ", "B  ", "C  ", "D  ", "E  ", "F  ", "G  ", "H  ") foreach print
			println("\n\t\tWhite Player (1)")
		}
		printBoard(board)
	}
	
	def CheckersController (Board: Array[Array[String]], input: String, turn: Boolean):Boolean = {
		
		val player = if (turn) -1 else 1
		
		val pattern = "([1-8])([a-hA-H])([1-8])([a-hA-H])".r
		var (fr, fc, tr, tc) = (0, 0, 0, 0)
		
		/* Pattern Matcher */
		def matchInput(in: String): Int = in match {
			case pattern(_*) =>
				fr = '8' - in(0)
				fc = in(1).toLower - 'a'
				tr = '8' - in(2)
				tc = in(3).toLower - 'a'
				validMove(fr,fc,tr,tc)
			case _ => -1
		}
		
		def checkEmptyCell(a: Char) = a.isSpaceChar
		
		def checkAlly(a: Char) = a.isLower && turn || a.isUpper && !turn
		
		def checkOpponent(x: Int, y: Int): Boolean = {
			if (x >= 0 && x < 8 && y >= 0 && y < 8)
				!checkEmptyCell(Board(x)(y)(1)) && !checkAlly(Board(x)(y)(1))
			else
				false
		}
		def checkValidPieceSelection(fromX:Int, fromY: Int): Boolean = checkAlly(Board(fromX)(fromY)(1))
		
		def checkEmptyCellMovement(toX:Int, toY:Int): Boolean = checkEmptyCell(Board(toX)(toY)(1))
		
		def checkCapturePosition(x: Int, y: Int):Boolean = {
			
			val king = if (Board(x)(y)(1) == 'k' || Board(x)(y)(1) == 'K') true else false
			
			if (checkOpponent(x-1, y-1) && x > 1 && y > 1 && checkEmptyCell(Board(x-2)(y-2)(1)) && (turn || king)) true
			
			else if (checkOpponent(x-1, y+1) && x > 1 && y < 6 && checkEmptyCell(Board(x-2)(y+2)(1)) && (turn || king)) true
			
			else if (checkOpponent(x+1, y-1) && x < 6 && y > 1 && checkEmptyCell(Board(x+2)(y-2)(1)) && (!turn || king)) true
			
			else if (checkOpponent(x+1, y+1) && x < 6 && y < 6 && checkEmptyCell(Board(x+2)(y+2)(1)) && (!turn || king)) true
			
			else false
		}
		
		def checkNoCaptureAvoidance(x:Int, y:Int):Boolean = {
			var validPieces: Set[(Int, Int)] = Set()
			
			for (i <- Board.indices){
				for (j <- Board.indices){
					if (checkAlly(Board(i)(j)(1))){
						
						if (checkCapturePosition(i, j)){
							val piece = (i, j)
							validPieces = validPieces + piece
						}
						
					}
				}
			}
			
			validPieces.isEmpty || validPieces.contains((x,y))
		}
		def checkMovement(x1: Int, y1: Int, x2: Int, y2: Int): (Boolean, Boolean) = {
			
			val king = if (Board(x1)(y1)(1) == 'k' || Board(x1)(y1)(1) == 'K') true else false
			
			var valid = false
			var kill = false
			
			val rightUpMove = checkOpponent(x1-1, y1+1) && x1 > 1 && y1 < 6 && checkEmptyCell(Board(x1-2)(y1+2)(1))
			
			val leftUpMove = checkOpponent(x1-1, y1-1) && x1 > 1 && y1 > 1 && checkEmptyCell(Board(x1-2)(y1-2)(1))
			
			val rightDownMove = checkOpponent(x1+1, y1+1) && x1 < 6 && y1 < 6 && checkEmptyCell(Board(x1+2)(y1+2)(1))
			
			val leftDownMove = checkOpponent(x1+1, y1-1) && x1 < 6 && y1 > 1 && checkEmptyCell(Board(x1+2)(y1-2)(1))
			
			if (turn || king){
				if (rightUpMove && (x2-x1, y2-y1) == (-2, 2) || leftUpMove && (x2-x1, y2-y1) == (-2, -2)) {
					Board(x2)(y2)= Board(x1)(y1)
					Board(x1)(y1) = s"${Board(x1)(y1)(0)} "
					valid = true
					kill = true
					if (rightUpMove)
						Board(x1-1)(y1+1) = s"${Board(x1-1)(y1+1)(0)} "
					else if (leftUpMove)
						Board(x1-1)(y1-1) = s"${Board(x1-1)(y1-1)(0)} "
				}
			}
			if (!turn || king && !valid){
				if (rightDownMove && (x2-x1, y2-y1) == (2, 2) || leftDownMove && (x2-x1, y2-y1) == (2, -2)) {
					Board(x2)(y2)= Board(x1)(y1)
					Board(x1)(y1) = s"${Board(x1)(y1)(0)} "
					valid = true
					kill = true
					if (rightDownMove)
						Board(x1+1)(y1+1) = s"${Board(x1+1)(y1+1)(0)} "
					else if (leftDownMove)
						Board(x1+1)(y1-1) = s"${Board(x1+1)(y1-1)(0)} "
				}
			}
			
			if (turn && !valid && (x2-x1, Math.abs(y2-y1)) == (-1, 1) && x2 > -1 && x2 < 8 && y2 > -1 && y2 < 8){
				Board(x2)(y2)= Board(x1)(y1)
				Board(x1)(y1) = s"${Board(x1)(y1)(0)} "
				valid = true
			}
			else if (!turn && !valid && (x2-x1, Math.abs(y2-y1)) == (1, 1)&& x2 > -1 && x2 < 8 && y2 > -1 && y2 < 8){
				Board(x2)(y2) = Board(x1)(y1)
				Board(x1)(y1) = s"${Board(x1)(y1)(0)} "
				valid = true
			}
			else if (king && !valid && (Math.abs(x2-x1), Math.abs(y2-y1)) == (1, 1) && x2 > -1 && x2 < 8 && y2 > -1 && y2 < 8){
				Board(x2)(y2)= Board(x1)(y1)
				Board(x1)(y1) = s"${Board(x1)(y1)(0)} "
				valid = true
			}
			
			(valid, kill)
		}
		
		def validMove(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
			
			var valid = checkValidPieceSelection(x1, y1) && checkEmptyCellMovement(x2, y2) && checkNoCaptureAvoidance(x1, y1)
			var cont = false
			
			if (valid){
				val (moved, kill) = checkMovement(x1, y1, x2, y2)
				valid = moved
				if (x2 == 7 && valid) Board(x2)(y2) =  s"${Board(x2)(y2)(0)}K"
				else if (x2 == 0 && valid) Board(x2)(y2) =  s"${Board(x2)(y2)(0)}k"
				cont = valid && kill && checkCapturePosition(x2, y2)
				
			}
			if (valid && !cont) 1 else if (valid && cont) 0 else -1
		}
		
		val BoardCopy = Board.map(_.clone())
		val inputs = input.split(" ")
		val result = inputs.map(x => matchInput(x))
		var ans = result.last == 1
		ans = ans && result.dropRight(1).forall(_ == 0)
		
		if (!ans) {
			for (i <- Board.indices){
				for (j <- Board.indices){
					Board(i)(j) = BoardCopy(i)(j)
				}
			}
			false
			}
		else true
	}

}