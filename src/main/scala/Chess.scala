import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.control.Breaks._

object Chess extends App{
  var board = Array(
    Array("-R",".N","-B",".Q","-K",".B","-N",".R"),
    Array(".P","-P",".P","-P",".P","-P",".P","-P"),
    Array("- ",". ","- ",". ","- ",". ","- ",". "),
    Array(". ","- ",". ","- ",". ","- ",". ","- "),
    Array("- ",". ","- ",". ","- ",". ","- ",". "),
    Array(". ","- ",". ","- ",". ","- ",". ","- "),
    Array("-p",".p","-p",".p","-p",".p","-p",".p"),
    Array(".r","-n",".b","-q",".k","-b",".n","-r") )

  def getDrawer: Array[Array[String]] => Unit = ChessDrawer
  def getController: (Array[Array[String]], String, Boolean) => Boolean = ChessController
  def getBoard: Array[Array[String]] = board

  def ChessDrawer(board: Array[Array[String]]): Unit = {
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
      //print the chess board
      println("\nThe Chess Board:")
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

  def ChessController(board: Array[Array[String]],
                      input: String,
                      turn: Boolean): Boolean =
  {
    val pattern = "([1-8])([a-hA-H])([1-8])([a-hA-H])".r
    var (fr, fc, tr, tc) = (0, 0, 0, 0)
    var promotion = false
    //local vars for king checks
    var wcheck = 0
    var bcheck = 0
    /* Pattern Matcher */
    def matchInput(in: String): Boolean = in match {
      case pattern(_*) =>
        fr = '8' - in(0)
        fc = in(1).toLower - 'a'
        tr = '8' - in(2)
        tc = in(3).toLower - 'a'
        validMove()
      case _ => false
    }

    def Empty(a: Char) = a.isSpaceChar
    def Ally(a: Char) = a.isLower && turn || a.isUpper && !turn
    def validPawnMove(): Boolean = {
      val (dx, dy) = (tc - fc, tr - fr)
      if(( turn && tr == 0 && input.length() != 5)
      || (!turn && tr == 7 && input.length() != 5)){ // a check to prevent moving to last square without promotion
        return false
      }
      //Pawn kill
      if(( turn && dx.abs == 1 && dy == -1)
      || (!turn && dx.abs == 1 && dy ==  1)){
        if ( !Ally(board(tr)(tc)(1)) ){
          true
        } else{
          false
        }
      }else{ // normal move
        if ( !Empty(board(tr)(tc)(1)) && !Ally(board(tr)(tc)(1))){//making normal move doesn't kill
          return false
        }
        if ( turn && fr == 6 || !turn && fr == 1){ //the initial position
          if(((dy == -1 || dy == -2) && dx == 0 && turn)
          || ((dy ==  1 || dy ==  2) && dx == 0 && !turn)){
            true
          } else{
            false
          }
        } else{ // not in initial position
          if(( turn && dy == -1 && dx == 0)
          || (!turn && dy ==  1 && dx == 0)){
            true
          } else{
            false
          }
        }
      }
    }
    def notRookJump(): Boolean = { // return true when there`s no thing blocking the piece
      val dy = tr - fr
      val dx = tc - fc
      // rook jumping
      if (dx > 0){ // right move
        for (i<- fc + 1 until tc) {
          if ( !Empty(board(fr)(i)(1)) ){ // not empty
            return false
          }
        }
      }
      else if (dx < 0) {//left move
        for (i<- fc-1 to tc+1 by -1) {
          if ( !Empty(board(fr)(i)(1)) ){ // not empty
            return false
          }
        }
      }
      else if ( dy < 0 ){// up move decrease in y
        for (i<- fr-1 to tr+1 by -1) { // I= 6
          if ( !Empty(board(i)(fc)(1)) ){ // not empty
            return false
          }
        }
      }
      else if (dy > 0){// down move increase in y
        for (i<- fr + 1 until tr) {
          if ( !Empty(board(i)(fc)(1)) ){ // not empty
            return false
          }
        }
      }
      true

    }
    def notBishopJump(): Boolean = {
      val dy = tr - fr
      val dx = tc - fc
      //iy, ix to scan the diagonals around the bishop
      var (iy, ix) = (fr, fc)
      if (dx > 0 && dy < 0){// dx+ dy-  up right move
        ix = fc+1; iy = fr-1
        while(ix < tc){
          if ( !Empty(board(iy)(ix)(1)) ){ // not empty
            return false
          }
          ix += 1; iy -= 1
        }
      }
      else if (dx > 0 && dy > 0){// dx+ dy+  down right move
        ix = fc+1; iy = fr+1
        while(ix < tc){
          if ( !Empty(board(iy)(ix)(1)) ){ // not empty
            return false
          }
          ix += 1; iy += 1
        }
      }
      else if (dx < 0 && dy < 0){// dx- dy-  up left move
        ix = fc-1; iy = fr-1 ;
        while(ix > tc){
          if ( !Empty(board(iy)(ix)(1)) ){ // not empty
            return false
          }
          ix -= 1; iy -= 1
        }
      }
      else if (dx < 0 && dy > 0){// dx- dy+ down left move
        ix = fc-1; iy = fr+1
        while(ix > tc){
          if ( !Empty(board(iy)(ix)(1)) ){ // not empty
            return false
          }
          ix -= 1; iy += 1
        }
      }
      true
    }
    @tailrec
    def kingLocation(turn: Boolean, y: Int, x: Int): (Int, Int) = {
      if( turn && board(y)(x)(1) == 'k'
      || !turn && board(y)(x)(1) == 'K') (y, x)
      else if(x < 7) kingLocation(turn, y, x+1)
      else if(x == 7 && y < 7) kingLocation(turn, y+1, 0)
      else (-1, -1)
    }
    def findChecks(): Unit= {
      wcheck = 0; bcheck = 0
      val (y, x) = kingLocation(turn, 0, 0)
      println(s"King = '${board(y)(x)(1)}' in $y-$x")
      val directions = Array(Array(-1, 0),Array(0, -1),Array(1, 0),Array(0, 1),   //4 rook directions(orthogonal)
                             Array(-1,-1),Array(-1, 1),Array(1,-1),Array(1, 1));  //4 bishop directions(diagonal)
      var next_y = 0; var next_x = 0
      var next_square = ' '
      for(i<- board.indices){
        val pin = Array(-1, -1, -1)
        breakable
        {for(j<- 1 until board.length){
          next_y = y + directions(i)(0) * j
          next_x = x + directions(i)(1) * j
          if((0 until 8 contains next_x) && (0 until 8 contains next_y)){ //inside the board
            next_square = board(next_y)(next_x)(1)
            if(Ally(next_square) && next_square.toLower != 'k'){ //ally case
              if(pin(0) == -1){ //get 1st ally in this direction as a possible pinned piece
                pin(0) = next_square
                pin(1) = next_y
                pin(2) = next_x
              }
              else{ //This direction is safe by an ally
                break()
              }
            }
            else if(!Empty(next_square) && !Ally(next_square)){ //enemy case
              println(s"enemy = $next_square in $next_y-$next_x")
              if((0 <= i && i <= 3 && next_square.toLower == 'r') //rook in orthogonal direction
              || (4 <= i && i <= 7 && next_square.toLower == 'b') //bishop in diagonal direction
              || (next_square.toLower == 'q') //queen in any direction
              //pawn in his attacking position
              || (j == 1 && next_square.toLower == 'p' && ((next_square == 'p' && 6 <= i && i <= 7) || (next_square == 'P' && 4 <= i && i <= 5)))
              || (j == 1 && next_square.toLower == 'k')){ //the other king
                if(pin(0) == -1){
                  if(turn){ //white king in check
                    wcheck += 1
                  }
                  else{ //black king in check
                    bcheck += 1
                  }

                }
              }
              else{ //enemy doesn't check the king
                break()
              }
            }
          }
          else{ //outside the board
            break()
          }
        }}
      }
      //knight causing a check (has special moves)
      val knight_moves = Array(Array(-1, -2), Array(-1, 2), Array(1, -2), Array(1, 2),
                               Array(-2, -1), Array(-2, 1), Array(2, -1), Array(2, 1))
      for(i<- board.indices){
        next_y = y + knight_moves(i)(0)
        next_x = x + knight_moves(i)(1)
        if((0 until 8 contains next_x) && (0 until 8 contains next_y)){ //inside the board
          next_square = board(next_y)(next_x)(1)
          if(!Ally(next_square) && next_square.toLower == 'n'){
            if(turn){ //white king in check by knight
              wcheck += 1
            }
            else{ //black king in check by knight
              bcheck += 1
            }
          }
        }
      }
      println(s"checks: w=$wcheck, b=$bcheck")
    }
    def moveValidForChecks(): Boolean = {
      val temp = board(tr)(tc)(1)
      board(tr)(tc) = s"${board(tr)(tc)(0)}${board(fr)(fc)(1)}"
      board(fr)(fc) = s"${board(fr)(fc)(0)} "
      findChecks() //see if the move will not cause a check or will remove the check(if any)
      //undo move
      board(fr)(fc) = s"${board(fr)(fc)(0)}${board(tr)(tc)(1)}"
      board(tr)(tc) = s"${board(tr)(tc)(0)}$temp"
      if(wcheck > 0 && turn || bcheck > 0 && !turn)
        false
      else
        true
    }
    /* Validator */
    def validMove(): Boolean = {
      val from = board(fr)(fc)(1)
      val to   = board(tr)(tc)(1)
      val dy = tr - fr //difference between rows (Vertical)
      val dx = tc - fc //difference between cols (Horizontal)
      if(from.isUpper && turn || from.isLower && !turn //playing with other player's pieces
        || !Empty(to) && Ally(to) || Empty(from)){ //hitting an ally || moving nothing
        false
      }
      /**
       * Special Moves
       */
      //promotion
      else if(input.length() == 5){
        val piece = input(4).toLower
        if(piece == 'q' || piece == 'r' || piece == 'n' || piece == 'b'){
          if (from.toLower == 'p'){
            if ( validPawnMove() && (turn && tr == 0 || !turn && tr == 7)){
              //convert the promoted pawn
              board(fr)(fc) = if(turn) s"${board(fr)(fc)(0)}${piece.toLower}"
                              else     s"${board(fr)(fc)(0)}${piece.toUpper}"
              if(moveValidForChecks()){
                promotion = true
                true
              }
              else{
                //undo promotion
                board(fr)(fc) = if(turn) s"${board(fr)(fc)(0)}p"
                                else     s"${board(fr)(fc)(0)}P"
                false
              }
            }
            else false
          }
          else false
        }
        else false
      }
      /* Constraints of the pieces */
      //Bishop
      else if(from.toLower == 'b'){
        if(dx.abs == dy.abs && notBishopJump()){
          moveValidForChecks()
        }
        else false
      }
      //Rook
      else if(from.toLower == 'r'){
        if((dx == 0 || dy == 0) && notRookJump()){
          moveValidForChecks()
        }
        else false
      }
      //Queen
      else if(from.toLower == 'q'){
        if(dx.abs == dy.abs && notBishopJump()    //bishop-like move
          ||(dx == 0 || dy == 0) && notRookJump()){ //rook-like move
          moveValidForChecks()
        }
        else false
      }
      //Knight
      else if(from.toLower == 'n'){
        if((dx.abs == 1 && dy.abs == 2) || (dx.abs == 2 && dy.abs == 1)){
          moveValidForChecks()
        }
        else false
      }
      //Pawn
      else if(from.toLower == 'p'){
        //En-passant...

        if(validPawnMove()){
          moveValidForChecks()
        }
        else false
      }
      //King
      else if(from.toLower == 'k'){
        if(dx.abs <= 1 && dy.abs <= 1){
          //if king goes to other king's region
          val (ky, kx) = kingLocation(!turn, 0, 0) //get the "other" king location
          if((tr, tc) == (ky, kx-1) || (tr, tc) == (ky-1, kx-1)
          || (tr, tc) == (ky-1, kx) || (tr, tc) == (ky-1, kx+1)
          || (tr, tc) == (ky, kx+1) || (tr, tc) == (ky+1, kx+1)
          || (tr, tc) == (ky+1, kx) || (tr, tc) == (ky+1, kx-1)){
            false
          }
          else{
            moveValidForChecks()
          }
        }
        //Castling ...
        else false
      }
      else
        false
    }
    def performMove(): Unit = {
      //Doing the move
      board(tr)(tc) = s"${board(tr)(tc)(0)}${board(fr)(fc)(1)}"
      board(fr)(fc) = s"${board(fr)(fc)(0)} "
    }

    /* The controller sequence */
    val valid = matchInput(input)
    if(valid) { performMove(); true }
    else      { false }
  }

  var turn = true
  var input = ""
  /* The Game Loop. */
  while(true){
    ChessDrawer(board)
    print(if(turn) "Player 1's turn!" else "Player 2's turn!")
    input = readLine(" Enter the move: ")
    while(!ChessController(board, input, turn)){
      input = readLine("Invalid move! Enter the move: ")
    }
    turn = !turn
  }
}