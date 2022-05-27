import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.HBox

import java.io.File
import scala.language.postfixOps
import scala.sys.process._

object Launcher extends JFXApp3{
  override def start(): Unit = {

    stage = new JFXApp3.PrimaryStage {
      title = "Game Launcher"
      scene = new Scene {
        root = new HBox {
          children = Seq(
            new Button {
              text = "Start"
              onAction = _ => {
                //val process = new ProcessCreation {}
                var process = "D:\\Study Stuff\\Year 2\\2nd Semester\\Programming Paradigms\\Assignments\\Assignment 3\\Game-Engine\\out\\artifacts\\Games_Launcher_jar\\run.bat"!
                  //println(process);
                //val processB = process.apply("cmd /c 'D:/Study Stuff/Year 2/2nd Semester/Programming Paradigms/Assignments/Assignment 3/Game-Engine/out/artifacts/Games_Launcher_jar/run.bat'");
                //processB.run()
                //close()
              };
            }
          )
        }
      }
    }
  }
}
