import scala.annotation.tailrec
import scala.util.control.Breaks._

object Chess {
  var board: Array[Array[String]] = Array(
    Array("-R",".N","-B",".Q","-K",".B","-N",".R"),
    Array(".P","-P",".P","-P",".P","-P",".P","-P"),
    Array("- ",". ","- ",". ","- ",". ","- ",". "),
    Array(". ","- ",". ","- ",". ","- ",". ","- "),
    Array("- ",". ","- ",". ","- ",". ","- ",". "),
    Array(". ","- ",". ","- ",". ","- ",". ","- "),
    Array("-p",".p","-p",".p","-p",".p","-p",".p"),
    Array(".r","-n",".b","-q",".k","-b",".n","-r") )

  def getDrawer: Array[Array[String]] => String = ChessDrawer
  def getController: (Array[Array[String]], String, Boolean) => Boolean = ChessController
  def getBoard: Array[Array[String]] = board

  def ChessDrawer(board: Array[Array[String]]): String = {
    /* Prints the whole board with its content in a square format. */
    def printBoard(b: Array[Array[String]]): String = {
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
      var sb = new StringBuilder
      sb.append("\nThe Chess Board:\n")
      sb.append("\t\tBlack Player\n")
      sb.append("    ")
      //println("\nThe Chess Board:")
      //println("\t\tBlack Player")
      //print("    ")
      //print A to H above the board
      List("A  ", "B  ", "C  ", "D  ", "E  ", "F  ", "G  ", "H  ") foreach sb.append
      sb.append("\n")
      //println()
      for(i<- b.indices){
        sb.append(s"${8-i}  " + bg1)
        //print(s"${8-i}  " + bg1);  //print 1 to 8 on the left side
        for(j<- b.indices){
          sb.append(s"${if(b(i)(j)(0) == '-' || b(i)(j)(0) == '_') bg1 else bg2}" +
            s" ${if(b(i)(j)(1).isLower) p1 else p2}${b(i)(j)(1)} ")
          /*print(s"${if(b(i)(j)(0) == '-' || b(i)(j)(0) == '_') bg1 else bg2}" +
            s" ${if(b(i)(j)(1).isLower) p1 else p2}${b(i)(j)(1)} ")*/
        }
        sb.append(s"$reset  ${8-i}\n")
        //println(s"$reset  ${8-i}"); //print 1 to 8 on the right side
      }
      sb.append("    ")
      //print("    ")
      //print A to H below the board
      List("A  ", "B  ", "C  ", "D  ", "E  ", "F  ", "G  ", "H  ") foreach sb.append
      sb.append("\n\t\tWhite Player")
      //println("\n\t\tWhite Player")
      var s = new String(sb.toString())
      return s
    }
    return printBoard(board)
  }

  def ChessController(board: Array[Array[String]], input: String, turn: Boolean): Boolean =
  {
    val pattern = "([1-8])([a-hA-H])([1-8])([a-hA-H])([nbrq]?)".r
    var (fr, fc, tr, tc) = (0, 0, 0, 0)
    val promotionInfo: Array[Char] = Array(' ', ' ')
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

    def Empty(y: Int, x: Int) = board(y)(x)(1).isSpaceChar
    def Ally(a: Char) = a.isLower && turn || a.isUpper && !turn
    def notMoved(y: Int, x: Int) = board(y)(x)(0) == '-' || board(y)(x)(0) == '.'
    def castlingMove(): Boolean = {
      val (dx, dy) = (tc - fc, tr - fr)
      //white king in k{7,4}
      if(turn && dx.abs == 2 && dy == 0 && notMoved(7, 4)){//king didn't move before
        if(kingNotInCheck()){//king not in check
          //short castling
          if(dx == 2 && Empty(7, 5) && Empty(7, 6) //empty places between king & rook
            && notMoved(7, 7) && moveValidForChecks() //rook didn't move before & no check in the pass of king
            && board(7)(7)(1) == 'r'){ //rook not in grav
            return true
          }
          //long castling
          else if(dx == -2 && Empty(7, 3) && Empty(7, 2) && Empty(7, 1) //empty places between king & rook
            && notMoved(7, 0) && moveValidForChecks() //rook didn't move before & no check in the pass of king
            && board(7)(0)(1) == 'r'){ //rook not in grav
            return true
          }
        }
      }
      //black king in K{0,4}
      else if(!turn && dx.abs == 2 && dy == 0 && notMoved(0, 4)){//king didn't move before
        if(kingNotInCheck()){//king not in check
          //short castling
          if(dx == 2 && Empty(0, 5) && Empty(0, 6) //empty places between king & rook
            && notMoved(0, 7) && moveValidForChecks() //rook didn't move before & no check in the pass of king
            && board(0)(7)(1) == 'R'){ //rook not in grav
            return true
          }
          //long castling
          else if(dx == -2 && Empty(0, 3) && Empty(0, 2) && Empty(0, 1) //empty places between king & rook
            && notMoved(0, 0) && moveValidForChecks() //rook didn't move before & no check in the pass of king
            && board(0)(0)(1) == 'R'){ //rook not in grav
            return true
          }
        }
      }
      false
    }
    def validPawnMove(): Boolean = {
      val (dx, dy) = (tc - fc, tr - fr)
      if(( turn && tr == 0 && input.length() != 5)
        || (!turn && tr == 7 && input.length() != 5)){ // a check to prevent moving to last square without promotion
        return false
      }
      //Pawn kill
      if(( turn && dx.abs == 1 && dy == -1)
        || (!turn && dx.abs == 1 && dy ==  1)){
          if ( !Empty(tr, tc) && !Ally(board(tr)(tc)(1)) ){
            true
          } else{
            false
        }
      }else{ // normal move
        if ( !Empty(tr, tc) && !Ally(board(tr)(tc)(1))){//making normal move doesn't kill
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
          if ( !Empty(fr, i) ){ // not empty
            return false
          }
        }
      }
      else if (dx < 0) {//left move
        for (i<- fc-1 to tc+1 by -1) {
          if ( !Empty(fr, i) ){ // not empty
            return false
          }
        }
      }
      else if ( dy < 0 ){// up move decrease in y
        for (i<- fr-1 to tr+1 by -1) { // I= 6
          if ( !Empty(i, fc) ){ // not empty
            return false
          }
        }
      }
      else if (dy > 0){// down move increase in y
        for (i<- fr + 1 until tr) {
          if ( !Empty(i, fc) ){ // not empty
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
          if ( !Empty(iy, ix) ){ // not empty
            return false
          }
          ix += 1; iy -= 1
        }
      }
      else if (dx > 0 && dy > 0){// dx+ dy+  down right move
        ix = fc+1; iy = fr+1
        while(ix < tc){
          if ( !Empty(iy, ix) ){ // not empty
            return false
          }
          ix += 1; iy += 1
        }
      }
      else if (dx < 0 && dy < 0){// dx- dy-  up left move
        ix = fc-1; iy = fr-1 ;
        while(ix > tc){
          if ( !Empty(iy, ix) ){ // not empty
            return false
          }
          ix -= 1; iy -= 1
        }
      }
      else if (dx < 0 && dy > 0){// dx- dy+ down left move
        ix = fc-1; iy = fr+1
        while(ix > tc){
          if ( !Empty(iy, ix) ){ // not empty
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
    def kingNotInCheck(): Boolean = {
      var wcheck = 0; var bcheck = 0
      val (y, x) = kingLocation(turn, 0, 0)
      //println(s"King = '${board(y)(x)(1)}' in $y-$x")
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
            else if(!Empty(next_y, next_x) && !Ally(next_square)){ //enemy case
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
      //println(s"checks: w=$wcheck, b=$bcheck")
      if(turn && wcheck == 0 || !turn && bcheck == 0) true //no checks, then true
      else false
    }
    def moveValidForChecks(): Boolean = {
      val temp = board(tr)(tc)(1)
      //do move
      board(tr)(tc) = s"${board(tr)(tc)(0)}${board(fr)(fc)(1)}"
      board(fr)(fc) = s"${board(fr)(fc)(0)} "
      //see if the move will not cause a check or will remove the check(if any)
      val valid = kingNotInCheck()
      //undo move
      board(fr)(fc) = s"${board(fr)(fc)(0)}${board(tr)(tc)(1)}"
      board(tr)(tc) = s"${board(tr)(tc)(0)}$temp"
      if(valid) true
      else false
    }
    /* Validator */
    def validMove(): Boolean = {
      val from = board(fr)(fc)(1)
      val to   = board(tr)(tc)(1)
      val dy = tr - fr //difference between rows (Vertical)
      val dx = tc - fc //difference between cols (Horizontal)
      if(from.isUpper && turn || from.isLower && !turn //playing with other player's pieces
        || !Empty(tr, tc) && Ally(to) || Empty(fr, fc)){ //hitting an ally || moving nothing
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
                promotionInfo(0) = 'T'
                promotionInfo(1) = piece
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
        else if(castlingMove() && moveValidForChecks()){
          val ty = if(turn) 7 else 0 //get the row index of castling
          if(dx == 2){ //short castling
            board(ty)(5) = s"${board(ty)(5)(0)}${board(ty)(7)(1)}"
            board(ty)(7) = s"${board(ty)(7)(0)} "
          }
          else if(dx == -2){ //long castling
            board(ty)(3) = s"${board(ty)(3)(0)}${board(ty)(0)(1)}"
            board(ty)(0) = s"${board(ty)(0)(0)} "
          }
          true
        }
        else false
      }
      else false
    }
    def performMove(): Unit = {
      //mark the square of the kings or the rooks if anyone of them moved for the first time
      //For castling
      if((board(fr)(fc)(1).toLower == 'k' || board(fr)(fc)(1).toLower == 'r')
        && notMoved(fr, fc)){
        board(fr)(fc) = if(board(fr)(fc)(0) == '-') s"_${board(fr)(fc)(1)}"
        else                        s",${board(fr)(fc)(1)}"
      }
      //Doing the move
      board(tr)(tc) = s"${board(tr)(tc)(0)}${board(fr)(fc)(1)}"
      board(fr)(fc) = s"${board(fr)(fc)(0)} "
    }

    /**
     * The Controller Flow
     */
    val valid = matchInput(input)
    if(valid) {
      performMove()
      if(!promotionInfo(0).isSpaceChar){ //if promotion happens
        println(s"\nPawn got promoted to '${promotionInfo(1)}'. Nice job!")
      }
      true
    }
    else false
  }
}