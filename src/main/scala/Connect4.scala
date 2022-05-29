object Connect4 {
    var Board :Array[Array[String]]=Array(
      Array("▢","▢","▢","▢","▢","▢"),
      Array("▢","▢","▢","▢","▢","▢"),
      Array("▢","▢","▢","▢","▢","▢"),
      Array("▢","▢","▢","▢","▢","▢"),
      Array("▢","▢","▢","▢","▢","▢"),
      Array("▢","▢","▢","▢","▢","▢"),
      Array("▢","▢","▢","▢","▢","▢")
    )
  def getBoard: Array[Array[String]] = Board

  def getDrawer: Array[Array[String]] =>String = printBoard

  def printBoard(board: Array[Array[String]]):String ={
    var x = 1;
    var sb = new StringBuilder
    while (x<8) {
      sb.append(x + "  \t")
      //print(x + "  ")
      x +=1
    }
    sb.append("\n");
    //println()
    var f = 5
    while (f > -1) {
      var s = 0
      while (s < 7) {
        sb.append(board(s)(f) + "  \t")
        //print(board(s)(f) + "  ")
        s += 1
      }
      sb.append("\n")
      //println()
      f -= 1
    }
    sb.append("\n")
    //println()
    val s = new String(sb.toString())
    return s;
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
          if(board(f-1)(5)!="▢"){
             false
          }else{
            var free=0
            while(board(f-1)(free) != "▢") {
              free +=1
            }
            if(player)
              board(f-1)(free)= "◯"
            else
               board(f-1)(free)= "⬤"
            true
          }
        }
    }
  }
}
