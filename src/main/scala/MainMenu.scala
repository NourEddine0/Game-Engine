import Launcher.stage
import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.event.ActionEvent
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color.{Black, DarkGray, DarkRed, Red, White}
import scalafx.scene.paint.{Color, LinearGradient, Stops}
import scalafx.scene.text.Text

import scala.annotation.unused

object MainMenu extends JFXApp3{
  val RESET = "\u001b[0m" // Text Reset
  def getDrawer: Array[Array[String]] => Unit = MainMenuDrawer

  def getController: Engine = MainMenuController()

  def getBoard: Array[Array[String]] = {
    val board = Array.ofDim[String](4, 2)
    board(0)(0) = ""
    board(0)(1) = "1.Tic Tac Toe"
    board(1)(0) = ""
    board(1)(1) = "2.Chess"
    board(2)(0) = ""
    board(2)(1) = "3.Checkers"
    board(3)(0) = ""
    board(3)(1) = "4.Connect 4"
  board
  }
  def MainMenuDrawer(board: Array[Array[String]]): Unit ={
    println("---------------")
    println("Game launcher")
    println("---------------")

    def printFunc(i:Int): Unit = if(board(i)(0).equals(">")){println(board(i)(1));}
                          else{println(board(i)(1))}
    for(i <- board.indices){
     printFunc(i)
    }
    }


  def MainMenuController(): Engine ={
    var board: Array[Array[String]] = getBoard

    var drawer: Array[Array[String]] => Unit = getDrawer

    drawer(board)

    val in = scala.io.StdIn.readLine()
    if(in.equals("1")){

      val engine = new Engine(XO.getController, XO.getDrawer, XO.getBoard)
      //engine.startGame
      return engine
    }
    if(in.equals("2")){

      val engine = new Engine(Chess.getController, Chess.getDrawer, Chess.getBoard)
      //engine.startGame
      return engine

      //eng.startGame

    }
    if(in.equals("3")){

      //engine.startGame

      val engine = new Engine(Checkers.getController, Checkers.getDrawer, Checkers.getBoard)

      return engine//eng.startGame

    }

    if(in.equals("4")){

      val engine = new Engine(Connect4.getController, Connect4.getDrawer, Connect4.getBoard)
      //engine.startGame
      return engine
      //val eng = new Engine(Connect4.getController, Connect4.getDrawer, Connect4.getBoard)
      //eng.startGame
    }

    val engine = new Engine(XO.getController, XO.getDrawer, XO.getBoard)
    //engine.startGame
    return engine
  }
override def start(): Unit ={
/*
*/
  /*
  stage = new JFXApp3.PrimaryStage {
    //    initStyle(StageStyle.Unified)
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = Color.rgb(38, 38, 38)
      content = new HBox {
        padding = Insets(50, 80, 50, 80)
        children = Seq(


          }
        )
      }
    }
  }*/

  stage = new JFXApp3.PrimaryStage {

    title = "Game Launcher"
    scene = new Scene (1000,500){

      var controller: Engine = getController
      val engine: Engine = controller;/*controller(engine);*/



      var board = new Text {
            text = engine.Drawer(engine.Board)
            fill = new LinearGradient(
              endX = 0,
              stops = Stops(Red, DarkRed))
          };
      board.layoutX = 300
      board.layoutY = 100

      val label = new Label("Move")

      var playerTurn= new Text("Player 1's turn!") {
        fill = new LinearGradient(
          endX = 0,
          stops = Stops(Black, DarkGray)
        )
      };
      playerTurn.layoutX = 10
      playerTurn.layoutY = 450
      playerTurn.style = "-fx-font: italic bold 10pt"

      var userInput: TextField = new TextField {
            promptText = "Enter your move"
            fill = new LinearGradient(
              endX = 0,
              stops = Stops(White, DarkGray)
            )
          };
      userInput.layoutX = 150
      userInput.layoutY = 450

      var gameStatus = new Text {
        fill = new LinearGradient(
          endX = 0,
          stops = Stops(Black, DarkGray)
        )
      };
      gameStatus.layoutX = 300
      gameStatus.layoutY = 470
      gameStatus.style = "-fx-font: italic bold 10pt sans-serif"

      var turn = true;

      userInput.onAction = (e)=>{
        var move = userInput.text.value ;
        if (engine.validateMove(move, gameStatus, playerTurn, turn)){
          turn = !turn
        }
        board.text = engine.Drawer(engine.Board)

      }
      content = List(board, userInput, gameStatus, playerTurn)
  }
}
}
}


