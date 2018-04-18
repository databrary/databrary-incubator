/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.databrary.controls;

import java.util.ArrayList;
import java.util.Map;
import javafx.application.Application;
import static javafx.application.Application.launch;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import org.controlsfx.control.MasterDetailPane;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.control.textfield.TextFields;
import org.databrary.entity.Owner;
import org.databrary.entity.Volume;
import org.databrary.utils.UrlUtils;

/**
 *
 * @author Shakir
 */
public class VolumeView { //extends MasterDetailPane {

    // private final Node rootIcon = new ImageView(
    //        new Image(getClass().getResourceAsStream("volume.png"))
    // );
    // ClassLoader classLoader = getClass().getClassLoader();
    // String imageUrl = classLoader.getResource("images\"volume.png").toExternalForm();
    // ImageView rootIcon = new ImageView(new Image(imageUrl));;
    MasterDetailPane mdPane = new MasterDetailPane();
    //   private final Node containerIcon = new ImageView(
    //           new Image(getClass().getResourceAsStream("folder.png"))
    //   );
    ArrayList<String> listOfVolumes = new ArrayList<String>();

    public VBox createVolumeView() throws Exception {
        ClassLoader classLoader = getClass().getClassLoader();
        String imageUrl = classLoader.getResource("images/volume.png").toExternalForm();
        ImageView rootIcon = new ImageView(new Image(imageUrl));
        rootIcon.setFitHeight(20);
        rootIcon.setFitWidth(20);
        rootIcon.setPreserveRatio(true);

        for (String volumeName : UrlUtils.loadVolumesNamesFromFile().keySet()) {

            listOfVolumes.add(volumeName);

        }

        mdPane.setDividerPosition(.5);
        mdPane.setDetailSide(Side.RIGHT);
        mdPane.setShowDetailNode(true);
        mdPane.setStyle("-fx-border-color: grey");

        CustomTextField input = (CustomTextField) TextFields.createClearableTextField();
        input.setPromptText("Enter volume name");
        //input.setPrefWidth(300);
        input.setPrefColumnCount(30);

        //   TextField input = new TextField();
        Button search = new Button("Search");
        TextFields.bindAutoCompletion(input, listOfVolumes);
        VBox root = new VBox(13);
        root.setPadding(new Insets(10, 10, 10, 10));
        HBox hb = new HBox(10);

        // hb.setAlignment(Pos.CENTER);
        hb.getChildren().addAll(input, search);

        search.setOnAction((ActionEvent event) -> {
            if (root.getChildren().size() > 1) {
                root.getChildren().remove(0);
            }
            try {
                
                
              String volumeId = UrlUtils.getVolumeIdByName(input.getText());
                
                TreeItem<String> rootItem = new TreeItem<String>("Volume: " + volumeId, rootIcon);
                rootItem.setExpanded(true);

                TreeView<String> tree = new TreeView<String>(rootItem);

                tree.getSelectionModel().selectedItemProperty()
                        .addListener(new ChangeListener<TreeItem<String>>() {

                            @Override
                            public void changed(
                                    ObservableValue<? extends TreeItem<String>> observable,
                                    TreeItem<String> old_val, TreeItem<String> new_val) {
                                TreeItem<String> selectedItem = new_val;
                                // l.setText(selectedItem.getValue());
                                String selectedValue = selectedItem.getValue();

                                if (selectedValue.startsWith("Volume:")) {
                                    System.out.println("volume selected");
                                    processVolume(volumeId);

                                } else if (selectedValue.startsWith("Volume:")) {
                                    System.out.println("Session selected");
                                    // processSession(volumeId);

                                }

                            }

                        });

                mdPane.setMasterNode(tree);

                System.out.println("volumeId: " + volumeId);

                Map<String, String> container = UrlUtils.getVolumeContainersNamesAndIds(volumeId);

                //for (String containerId : UrlUtils.getVolumeContainersNamesIds(volumeId).keySet()) {
                for (String containerId : container.keySet()) {

                    String folderUrl = classLoader.getResource("images/folder.png").toExternalForm();
                    ImageView containerIcon = new ImageView(new Image(folderUrl));
                    containerIcon.setFitHeight(20);
                    containerIcon.setFitWidth(20);
                    containerIcon.setPreserveRatio(true);

                    // Node containerIcon = new ImageView(new Image(getClass().getResourceAsStream("folder.png")));
                    TreeItem<String> containerItem = new TreeItem<String>("Session: " + container.get(containerId), containerIcon);
                    for (String fileId : UrlUtils.getFileNamesInSession(containerId)) {

                        String fileUrl = classLoader.getResource("images/file.png").toExternalForm();
                        ImageView fileIcon = new ImageView(new Image(fileUrl));
                        fileIcon.setFitHeight(18);
                        fileIcon.setFitWidth(18);
                        fileIcon.setPreserveRatio(true);

                        //  Node fileIcon = new ImageView(new Image(getClass().getResourceAsStream("file.png")));
                        TreeItem<String> fileItem = new TreeItem<String>(fileId, fileIcon);

                        containerItem.getChildren().add(fileItem);
                    };
                    rootItem.getChildren().add(containerItem);
                }
                root.getChildren().clear();
                root.getChildren().addAll(hb, mdPane);

            } catch (Exception e) {
                e.printStackTrace();
            }

        }
        );

        //root.getChildren().addAll(hb, tree);
        root.getChildren().addAll(hb);

        return root;
        //primaryStage.setScene(new Scene(root, 900, 450));
        //primaryStage.show();
        //input.setPrefWidth(primaryStage.getWidth() / 2.5);
    }

    public void processVolume(String volume_id) {

        try {
            Volume volume = UrlUtils.getVolumeObjectById(volume_id);

            TextArea ta = new TextArea();
            ta.setWrapText(true);
            ta.setPadding(new Insets(5, 5, 5, 5));
            ta.setText(volume.getBody());

            TitledPane tp = new TitledPane("Volume body", ta);

            VBox vbVolumeDetail = new VBox(10);
            vbVolumeDetail.setPadding(new Insets(5, 5, 5, 5));

            HBox hbVolumeCreationDate = new HBox(10);
            Label labelVolumeCreation = new Label("Created: ");
            //TextField tfVolumeCreated = new TextField();
            Text tfVolumeCreated = new Text();
            tfVolumeCreated.setText(volume.getCreation());
            hbVolumeCreationDate.getChildren().addAll(labelVolumeCreation, tfVolumeCreated);

            HBox hbDoi = new HBox(10);
            Label labelDoi = new Label("DOI: ");
            Text tfDoi = new Text();
            tfDoi.setText(volume.getDoi());
            hbDoi.getChildren().addAll(labelDoi, tfDoi);

            HBox hbOwners = new HBox(10);
            Label labelOwners = new Label("Owners: ");
            Text labelOwnersNames = new Text();
            labelOwnersNames.setText(volume.getDoi());

            StringBuilder owners = new StringBuilder();
            for (Owner owner : volume.getOwner()) {
                owners.append(owner.getName() + "; ");

            }
            labelOwnersNames.setText(owners.toString());

            hbOwners.getChildren().addAll(labelOwners, labelOwnersNames);

            HBox hbAccess = new HBox(10);
            Label labelAccess = new Label("Access:  ");
            Text tfAccess = new Text();
            tfAccess.setText(volume.getPublicaccess());
            hbAccess.getChildren().addAll(labelAccess, tfAccess);

            //  vbVolumeDetail.getChildren().addAll(tp, hbVolumeCreationDate, hbDoi, hbOwners);
            vbVolumeDetail.getChildren().addAll(tp, hbVolumeCreationDate, hbOwners, hbAccess);
            mdPane.setDetailNode(vbVolumeDetail);

            /*
             gridPane.add(text1, 0, 0); 
      gridPane.add(textField1, 1, 0); 
      gridPane.add(text2, 0, 1);       
      gridPane.add(textField2, 1, 1); 
      gridPane.add(button1, 0, 2); 
      gridPane.add(button2, 1, 2); 
             */
        } catch (Exception e) {
            e.printStackTrace();

        }

    }
}
