


object XO {
	def getDrawer: Array[Array[String]] => Unit = XODrawer
	def getController:(Array[Array[String]], String, Boolean) => Boolean = XOController
	def getBoard:Array[Array[String]] = {
		Array(Array(" ", " ", " "),
					Array(" ", " ", " "),
					Array(" ", " ", " "))
	}

	def XODrawer(board: Array[Array[String]]): Unit = {
		/* Prints the whole board with its content in a square format. */
		def printBoard(b: Array[Array[String]]): Unit = {
		println("    a   b   c")
		for(i <- b.indices){
			print(s"${b.length - i} | ")
			for(j<- b.indices){
			print(s"${b(i)(j)} | ")
			}
			println(s"${b.length - i}")
		}
		println("    a   b   c")
		}
		printBoard (board)
	}

	def XOController(board: Array[Array[String]], input:String, turn:Boolean): Boolean = {
		val pattern = "([1-3])([a-c|A-C])".r     /* Input Regex pattern */
		val XO_pattern = "(\\u001B\\[\\d+m)+(X)(\\u001B\\[\\d+m)+|(\\u001B\\[\\d+m)+(O)(\\u001B\\[\\d+m)+"
		var index1 = 0;             /* The parsed user input. */
		var index2 = 0
		/* Detects the matching of the input & returns a boolean value according
		to the matching result. */
		def matchInput(in: String): Boolean = in match {
			case pattern(_*) =>
				index1 = '3' - in(0)
				index2 = in(1).toLower - 'a'
				true
			case _ => false
		}

		//Resets the board if it's full
		def resetBoardIfFull(): Unit = {
			var counter = 0
			for (i <- board.indices) {
				for (j <- board.indices) {
					if(board(i)(j) != " ") counter += 1
				}
			}
			if(counter == 9){
				println(">>>>>>> Cleaning board....")
				println(">>>>>>> Board cleaned!")
				for (i <- board.indices) {
					for (j <- board.indices) {
						board(i)(j) = " "
					}
				}
			}
		}

		/**
		 * The Controller Flow
		 */
		resetBoardIfFull()
		/* if (the input does not match || the slot is not empty),
		then it's invalid input. */
		if (!matchInput(input)
			|| board(index1)(index2).matches(XO_pattern)) {
			false
		}
		else{ /* Performing the action. */
			board(index1)(index2) = if (turn) Console.RED+"X"+Console.RESET else Console.GREEN+"O"+Console.RESET
			true
		}
	}
}