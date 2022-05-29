
class Engine(val Controller: (Array[Array[String]], String, Boolean) => Boolean ,
             val Drawer: (Array[Array[String]]) => String, val Board: Array[Array[String]]) extends App{

  var turn = false

  def validateMove(input:String): Unit ={
    println(input)
    val in = takeInput(input);
    println(if(turn) "Player 1's turn!" else "Player 2's turn!")
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
  def startGame{

      this.Drawer(this.Board)
      var turn = true
      while (true){
        println(if(turn) "Player 1's turn!" else "Player 2's turn!")
        val in = takeInput(" ")
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

  def takeInput(input:String) : Either[Boolean, String] = {
    //val input = scala.io.StdIn.readLine("Enter your move: ")
    if(input.equalsIgnoreCase("exit"))
      Left(false)
    else
      Right(input)
  }
}