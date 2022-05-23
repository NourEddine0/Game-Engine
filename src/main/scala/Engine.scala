class Engine(val Controller: (String, Array[Array[Char]], Boolean) => Unit , val Drawer: (Array[Array[Char]]) => Unit){
  def startGame{
      while (true){
        val input = scala.io.StdIn.readLine("Enter your move: ")
      }

  }
}