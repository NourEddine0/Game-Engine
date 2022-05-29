//import scalafx.scene.control.{Label, TextField}
//import scalafx.application.JFXApp3
//import scalafx.scene._
//
//
//
//object ScalaFXHelloWorld extends JFXApp3 {
//
//  override def start(): Unit = {
//      stage = new JFXApp3.PrimaryStage {
//      title = "Game Launcher"
//      scene = new Scene(500,400){
//
//        val board = new Label()
//        board.layoutX = 180
//        board.layoutY = 80
//
//        val label = new Label("Enter")
//        label.layoutX = 10
//        label.layoutY = 350
//        label.style = "-fx-font: italic bold 10pt sans-serif"
//
//        val userInput = new TextField()
//        userInput.layoutX = 60
//        userInput.layoutY = 350
//        userInput.promptText = "Enter a move"
//
//        var gameStart = false
//
//        content = List(board, label, userInput)
//
//        val mainMenuBoard = MainMenu.getBoard
//        val mainMenuDrawer = MainMenu.getDrawer(mainMenuBoard, board)
//        val mainMenuController = MainMenu.getController
//
//        userInput.onAction = (e) =>{
//          if  (gameStart == false) {
//            mainMenuController(userInput.text.apply())
//            gameStart = true
//            print(userInput.text.apply())
//            }
//          }
//        }
//      }
//    }
//  }