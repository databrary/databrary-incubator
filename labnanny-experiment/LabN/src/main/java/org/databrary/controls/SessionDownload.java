/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.databrary.controls;

import javafx.application.Platform;
import javafx.geometry.Side;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import org.controlsfx.control.MasterDetailPane;

/**
 *
 * @author Shakir
 */
public class SessionDownload  extends MasterDetailPane{
    
    public SessionDownload(Stage stage) {
        SlotTree slotTree = new SlotTree();
        //MasterDetailPane pane = new MasterDetailPane();
        setMasterNode(slotTree);
       // pane.setDividerPosition(.35);
         setDividerPosition(.80);
        DownloadManager dm = new DownloadManager(slotTree);

        setDetailNode(dm);                  //pane.setDetailNode(new PropertySheet());
        setDetailSide(Side.BOTTOM);
        setShowDetailNode(true); 

//   Scene scene = new Scene(new SlotTree(), 600, 450);
        //Scene scene = new Scene(pane, 900, 450);
        //primaryStage.setScene(scene);
        //primaryStage.show();
        //slotTree.setPrefWidth(scene.getWidth()/4);

        stage.setOnCloseRequest((WindowEvent event1) -> {

            try {
                dm.stopThreadpool();
            } catch (Exception e) {
                e.printStackTrace();
            }
            Platform.exit();

        });
    
    
    }
    
    
    
}
