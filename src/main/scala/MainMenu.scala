

object MainMenu extends App{
  val RESET = "\u001b[0m" // Text Reset

  def getDrawer: Array[Array[String]] => Unit = MainMenuDrawer;
  def getController:(Array[Array[String]]) => Unit = MainMenuController;
  def getBoard(): Array[Array[String]] = {
    var board = Array.ofDim[String](4,2);
    board(0)(0) = "";
    board(0)(1) = "1.Tic Tac Toe"
    board(1)(0) = "";
    board(1)(1) = "2.Chess"
    board(2)(0) = "";
    board(2)(1) = "3.Checkers"
    board(3)(0) = "";
    board(3)(1) = "4.Connect 4"
  return board;
  }
  def MainMenuDrawer(board: Array[Array[String]]): Unit ={
    println(Console.YELLOW+"---------------");
    println("Game launcher");
    println("---------------"+Console.RESET);
    var i = 0
    def printFunc(i:Int): Unit = if(board(i)(0).equals(">")){println(board(i)(1));}
                          else{println(Console.GREEN+board(i)(1)+Console.RESET)}
    for(i <- 0 until board.length){
     printFunc(i);
      }
    }


  def MainMenuController(board: Array[Array[String]]): Unit ={
    var in = scala.io.StdIn.readLine();
    
    if(in.equals("1")){
      var eng = new Engine(XO.getController, XO.getDrawer, XO.getBoard())
      eng.startGame
    }
    if(in.equals("2")){
      var eng = new Engine(Chess.getController, Chess.getDrawer,Chess.getBoard)
      eng.startGame

    }
    if(in.equals("3")){
      var eng = new Engine(Checkers.getController, Checkers.getDrawer, Checkers.getBoard)
      eng.startGame

    }

    if(in.equals("4")){
      var eng = new Engine(Connect4.getController, Connect4.getDrawer, Connect4.getBoard)
      eng.startGame
    }
  }

  var board = getBoard();
  var drawer = getDrawer;
  var controller = getController;
  drawer(board);
  controller(board)
}


