import scala.io.StdIn.readLine
import scala.util.matching.Regex
import java.util.Scanner

object XO  {
	def getDrawer: Array[Array[Char]] => Unit = XODrawer;
	def getController:(Array[Array[Char]], String, Boolean) => Unit = XOController;
	def getBoard():Array[Array[Char]] ={
		var Board = Array.ofDim[Char](3, 3);
		return Board;
	} ; //The Board
	def XODrawer(board: Array[Array[Char]]): Unit = {
		/* Prints the whole board with its content in a square format. */
		def printBoard(b: Array[Array[Char]]): Unit = {
		println("    a   b   c")
		for(i <- 0 to b.length - 1){
			print(s"${b.length - i} | ")
			for(j<- 0 to b.length - 1){
			print(s"${b(i)(j)} | ")
			}
			println(s"${b.length - i}")
		}
		println("    a   b   c")
		}
		printBoard (board)
	}

  	def XOController(board: Array[Array[Char]], input:String, turn:Boolean): Unit = {
			/* Resets the board to be empty for the next game. */
			def resetBoard(): Unit = {
				for (i <- 0 to board.length - 1) {
					for (j <- 0 to board.length - 1) {
						board(i)(j) = ' ';
					}
				}
			}
			val pattern = "([1-3])([a-cA-C])".r     /* Input Regex pattern */
			var index1 = 0;             /* The parsed user input. */
			var index2 = 0;
			/* Detects the maching of the input & retuns a boolean value according
      to the matching result. */
			def matchInput(in: String): Boolean = in match {
				case pattern(_*) => {
					index1 = '3' - in(0);
					index2 = in(1) - 'a';
					true
				}
				case _ => false
			}			/* Reading Input Loop:
				if (the input does not match || the slot ia already filled),
				then reEnter the input. */

			if (!(matchInput(input))
				|| board(index1)(index2).toString.matches("X|O")) {
				/* Performing the action. */
				println("Invalid input")
				return false;


			}
			else{
				board(index1)(index2) = if (turn) 'X' else 'O'
				return true
			}
		}
}