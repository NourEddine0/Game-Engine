object Connect4 {
    var Board :Array[Array[String]]=Array(
      Array("○","○","○","○","○","○"),
      Array("○","○","○","○","○","○"),
      Array("○","○","○","○","○","○"),
      Array("○","○","○","○","○","○"),
      Array("○","○","○","○","○","○"),
      Array("○","○","○","○","○","○"),
      Array("○","○","○","○","○","○")
    )

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


  def getController:(Array[Array[String]],String,Int) => Boolean = {

    (board: Array[Array[String]], input: String,player:Int) => {
        var f=0
        try {
          f = input.toInt
        }catch {
            case _: Exception => f=10
        }

        if (f<1 || f>7) {
              false
        } else{
          if(board(f-1)(5)!="○"){
             false
          }else{
            var free=0
            while(board(f-1)(free) != "○") {
              free +=1
            }
            if(player==1)
              board(f-1)(free)= "●"
            else
               board(f-1)(free)= "◍"
            true
          }
        }
    }
  }

  def main(args: Array[String]): Unit ={
    var Board=Connect4.Board
    def drawer=Connect4.getDrawer
    drawer(Board)
    def controller=Connect4.getController
    println(getController(Board,"1",0))
    drawer(Board)
    println(getController(Board,"1",1))
    drawer(Board)
    println(getController(Board,"1",0))
    drawer(Board)
    println(getController(Board,"1",1))
    drawer(Board)
    println(getController(Board,"11",0))
    drawer(Board)
    println(getController(Board,"1",1))
    drawer(Board)
    println(getController(Board,"foo",1))
    drawer(Board)

  }


}
