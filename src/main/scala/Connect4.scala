object Connect4 {
    var Board :Array[Array[String]]=Array(
      Array("O","O","O","O","O","O"),
      Array("O","O","O","O","O","O"),
      Array("O","O","O","O","O","O"),
      Array("O","O","O","O","O","O"),
      Array("O","O","O","O","O","O"),
      Array("O","O","O","O","O","O"),
      Array("O","O","O","O","O","O")
    )
  def getBoard: Array[Array[String]] = Board
  def getDrawer: Array[Array[String]] =>Unit = {
      (board: Array[Array[String]]) => {
        var x = 1
        while (x<8) {
          print(x + "  ")
          x +=1
        }
        println()
        var f = 5
        while (f > -1) {
          var s = 0
          while (s < 7) {
            print(board(s)(f) + "  ")
            s += 1
          }
          println()
          f -= 1
        }
        println()
      }
    }


  def getController:(Array[Array[String]],String,Boolean) => Boolean = {

    (board: Array[Array[String]], input: String,player:Boolean) => {
        var f=0
        try {
          f = input.toInt
        }catch {
            case _: Exception => f=10
        }

        if (f<1 || f>7) {
              false
        } else{
          if(board(f-1)(5)!="O"){
             false
          }else{
            var free=0
            while(board(f-1)(free) != "O") {
              free +=1
            }
            if(player)
              board(f-1)(free)=Console.BLUE+"O"+Console.RESET
            else
               board(f-1)(free)= Console.YELLOW+"O"+Console.RESET
            true
          }
        }
    }
  }
}
