import scala.io.StdIn.readLine
import scala.util.matching.Regex
import java.util.Scanner


object XO {
	def getDrawer: Array[Array[String]] => Unit = XODrawer;
	def getController:(Array[Array[String]], String, Boolean) => Boolean = XOController;
	def getBoard():Array[Array[String]] ={
		var Board = Array.ofDim[String](3, 3);
		def resetBoard(): Unit = {
			for (i <- 0 until Board.length) {
				for (j <- 0 until Board.length) {
					Board(i)(j) = " ";
				}
			}
		}
		resetBoard()
		return Board;
	} ; //The Board
	def XODrawer(board: Array[Array[String]]): Unit = {
		/* Prints the whole board with its content in a square format. */
		def printBoard(b: Array[Array[String]]): Unit = {
		println("    a   b   c")
		for(i <- 0 until b.length){
			print(s"${b.length - i} | ")
			for(j<- 0 until b.length){
			print(s"${b(i)(j)} | ")
			}
			println(s"${b.length - i}")
		}
		println("    a   b   c")
		}
		printBoard (board)
	}

  	def XOController(board: Array[Array[String]], input:String, turn:Boolean): Boolean = {
			/* Resets the board to be empty for the next game. */

			val pattern = "([1-3])([a-cA-C])".r     /* Input Regex pattern */
			var index1 = 0;             /* The parsed user input. */
			var index2 = 0;
			/* Detects the matching of the input & returns a boolean value according
      to the matching result. */
			def matchInput(in: String): Boolean = in match {
				case pattern(_*) => {
					index1 = '3' - in(0);
					index2 = in(1).toLower - 'a';
					true
				}
				case _ => false
			}			/* Reading Input Loop:
				if (the input does not match || the slot ia already filled),
				then reEnter the input. */

			if (!(matchInput(input))
				|| board(index1)(index2).toString.matches("Console.RED+\"X\"+Console.RESET|Console.GREEN+\"O\"+Console.RESET")) {
				/* Performing the action. */
				println("Invalid input")
				false;


			}
			else{
				board(index1)(index2) = if (turn) Console.RED+"X"+Console.RESET else Console.GREEN+"O"+Console.RESET
				true
			}
		}
}