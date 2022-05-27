
class Engine(val Controller: (Array[Array[String]], String, Boolean) => Boolean ,
             val Drawer: (Array[Array[String]]) => Unit, val Board: Array[Array[String]]) extends App{


  def startGame{
      this.Drawer(this.Board)
      var turn = true
      while (true){
        println(if(turn) "Player 1's turn!" else "Player 2's turn!")
        val in = takeInput
        if(in.isLeft) {
          return
        }
        else {
          if (this.Controller(this.Board, in.fold(l => "false", r=>r), turn)) {
            this.Drawer(this.Board)
            turn = !turn
          }
          else {
            println("invalid move rejected")
          }
        }
      }
  }

  def takeInput : Either[Boolean, String] = {
    val input = scala.io.StdIn.readLine("Enter your move: ")
    if(input.equalsIgnoreCase("exit"))
      Left(false)
    else
      Right(input)
  }
}