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
import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
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
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SingleSelectionModel;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.ContextMenuEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import javafx.util.Callback;
import org.controlsfx.control.MasterDetailPane;
import org.controlsfx.control.textfield.AutoCompletionBinding;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.control.textfield.TextFields;
import org.databrary.entity.Asset;
import org.databrary.entity.Owner;
import org.databrary.entity.SessionInfo;
import org.databrary.entity.ToStringConverter;
import org.databrary.entity.Volume;
import org.databrary.utils.UrlUtils;

/**
 *
 * @author Shakir
 */
public class VolumeView extends VBox { //extends MasterDetailPane {

    String volumeId;
    public DownloadManagerVolume downloadManageVolume;
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
    SimpleStringProperty selectedAsset = new SimpleStringProperty("");
    // SimpleStringProperty selectedSession = new SimpleStringProperty("");

    public VolumeView() throws Exception {
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
        mdPane.setStyle("-fx-border-color: darkgrey");
        // mdPane.setStyle("-fx-background-color:grey");
        //-fx-background-color:

        CustomTextField input = (CustomTextField) TextFields.createClearableTextField();
        input.setPromptText("Enter volume name");
        input.setPrefColumnCount(30);
        AutoCompletionBinding<String> binding = TextFields.bindAutoCompletion(input, listOfVolumes);
        binding.minWidthProperty().bind(input.widthProperty());
        Button search = new Button("Search");

        //  VBox root = new VBox(13);
        setPadding(new Insets(10, 10, 10, 10));
        HBox hb = new HBox(10);
        VBox.setVgrow(mdPane, Priority.ALWAYS);
        // hb.setAlignment(Pos.CENTER);
        hb.getChildren().addAll(input, search);

        search.setOnAction((ActionEvent event) -> {
            if (getChildren().size() > 1) {
                getChildren().remove(0);
            }
            try {

                volumeId = UrlUtils.getVolumeIdByName(input.getText());

                TreeItem<String> rootItem = new TreeItem<String>("Volume: " + volumeId, rootIcon);
                rootItem.setExpanded(true);

                TreeView<String> tree = new TreeView<String>(rootItem);
                //   tree.setStyle("-fx-color:grey");
                // processAsset(tree);

                /*
                
                MenuItem item1 = new MenuItem("Preview");
                item1.setOnAction(new EventHandler<ActionEvent>() {
                    public void handle(ActionEvent e) {
                        mdPane.setDetailNode(null);
                        final WebView browser = new WebView();
                        final WebEngine webEngine = browser.getEngine();
                        // 00002.mp4 (1088 MB) |16891|73302
                        String sesId = UrlUtils.getSessionIdFromParser(selectedAsset.getValue());
                        System.out.println("sesId = " + sesId);

                        String assetId = UrlUtils.getAssetIdFromParser(selectedAsset.getValue());
                        System.out.println("assetId = " + assetId);

                        //webEngine.load("https://nyu.databrary.org/slot/6256/asset/9828");
                        String url = UrlUtils.urlBase + "slot/" + sesId + "/asset/" + assetId;
                        webEngine.load(url);
                        mdPane.setDetailNode(browser);

                        System.out.println("About");
                    }
                });
                final ContextMenu contextMenu = new ContextMenu(item1);
                UrlUtils.setCellFactory(tree, contextMenu);
                 */
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
                                    processVolume(tree, volumeId);

                                } else if (selectedValue.startsWith("Session:")) {
                                    System.out.println("Session selected = " + selectedValue.toString());
                                    String sesId = UrlUtils.getSessionId(selectedValue.toString());
                                    System.out.println("sessionId = " + sesId);
                                    processSession(tree, sesId);

                                } else {
                                    selectedAsset.setValue(selectedValue.toString());
                                    System.out.println("selected value = " + selectedAsset.getValue());

                                    processAsset(tree);

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
                    for (String fileName : UrlUtils.getFileNamesInSession(containerId)) {

                        String fileUrl = classLoader.getResource("images/file.png").toExternalForm();
                        ImageView fileIcon = new ImageView(new Image(fileUrl));

                        fileIcon.setFitHeight(18);
                        fileIcon.setFitWidth(18);
                        fileIcon.setPreserveRatio(true);

                        //  Node fileIcon = new ImageView(new Image(getClass().getResourceAsStream("file.png")));
                        TreeItem<String> fileItem = new TreeItem<String>(fileName, fileIcon);
                        System.out.println("fileId " + fileName);
                        containerItem.getChildren().add(fileItem);
                    };
                    rootItem.getChildren().add(containerItem);
                }
                getChildren().clear();
                getChildren().addAll(hb, mdPane);

            } catch (Exception e) {
                e.printStackTrace();
            }

        }
        );

        //root.getChildren().addAll(hb, tree);
        getChildren().addAll(hb);

        /* 
        getScene().getWindow().setOnCloseRequest((WindowEvent event1) -> {

            try {
               downloadManageVolume.stopThreadpool();
            } catch (Exception e) {
                e.printStackTrace();
            }
            Platform.exit();

        });
        
         */
        //primaryStage.setScene(new Scene(root, 900, 450));
        //primaryStage.show();
        //input.setPrefWidth(primaryStage.getWidth() / 2.5);
    }

    public void processVolume(TreeView<String> tree, String volume_id) {

        try {
            java.net.CookieHandler.setDefault(new java.net.CookieManager());
            mdPane.setDetailNode(null);
            Volume volume = UrlUtils.getVolumeObjectById(volume_id);

            TextArea ta = new TextArea();
            ta.setWrapText(true);
            ta.setPadding(new Insets(5, 5, 5, 5));
            ta.setText(volume.getBody());
            ta.setEditable(false);

            TitledPane tp = new TitledPane("Volume description", ta);

            VBox vbVolumeDetail = new VBox(10);
            vbVolumeDetail.setPadding(new Insets(5, 5, 5, 5));
            //vbVolumeDetail.setStyle("-fx-background-color: DAE6F3;");
            vbVolumeDetail.setStyle("-fx-background-color: lightgray;");

            HBox hbName = new HBox(10);
            Label labelName = new Label("Name:  ");
            //TextField tfVolumeCreated = new TextField();
            Text tfName = new Text();
            tfName.setText(volume.name);
            hbName.getChildren().addAll(labelName, tfName);

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
            vbVolumeDetail.getChildren().addAll(tp, hbName, hbVolumeCreationDate, hbOwners, hbAccess);
            mdPane.setDetailNode(vbVolumeDetail);

            MenuItem downloadVolume = new MenuItem("Download Volume");

            downloadVolume.setOnAction(new EventHandler<ActionEvent>() {
                public void handle(ActionEvent e) {
                    downloadManageVolume = new DownloadManagerVolume(volumeId);

                    getChildren().add(downloadManageVolume);

                }
            });

            final ContextMenu contextMenu = new ContextMenu(downloadVolume);
            UrlUtils.setCellFactory(tree, contextMenu);


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

    public void processSession(TreeView<String> tree, String sessionId) {

        try {
            java.net.CookieHandler.setDefault(new java.net.CookieManager());
            mdPane.setDetailNode(null);
            SessionInfo sesInfo = UrlUtils.getSessionInfo(sessionId);

            VBox vbSessionDetail = new VBox(18);
            //vbSessionDetail.setStyle("-fx-background-color: DAE6F3;");
            vbSessionDetail.setStyle("-fx-background-color: lightgray;");
            vbSessionDetail.setPadding(new Insets(5, 5, 5, 5));
            vbSessionDetail.setAlignment(Pos.TOP_CENTER);
            Label sessionDescription = new Label("Session description");

            //  vbVolumeDetail.getChildren().addAll(tp, hbVolumeCreationDate, hbDoi, hbOwners);
            GridPane grid = new GridPane();
            grid.setHgap(15);
            grid.setVgap(5);
            grid.setPadding(new Insets(0, 10, 0, 10));

            grid.add(new Label("Name:   "), 0, 0);
            grid.add(new Label(sesInfo.name), 1, 0);

            grid.add(new Label("Release date:"), 0, 1);
            grid.add(new Label(sesInfo.date), 1, 1);

            grid.add(new Label("Release:"), 0, 2);
            grid.add(new Label(sesInfo.release), 1, 2);

            grid.add(new Label("Id:"), 0, 3);
            grid.add(new Label(sesInfo.id), 1, 3);

            HBox hbGrid = new HBox(10);
            hbGrid.setAlignment(Pos.CENTER);
            hbGrid.getChildren().add(grid);
            /*
            HBox hbPreview = new HBox(10);
            hbPreview.setAlignment(Pos.CENTER);
            Button preview = new Button("Preview");
            hbGrid.getChildren().add(preview);
             */

            vbSessionDetail.getChildren().addAll(sessionDescription, hbGrid);

            mdPane.setDetailNode(vbSessionDetail);

            MenuItem downloadSession = new MenuItem("Download session");
            downloadSession.setOnAction(new EventHandler<ActionEvent>() {
                public void handle(ActionEvent e) {

                    // downloads.setContent(new SessionDownload(mainStage));
                    System.out.println("shakir scene " + ((BorderPane) tree.getScene().getRoot()));

                    Scene scene = tree.getScene();
                    Stage stage = (Stage) scene.getWindow();

                    System.out.println("stage  " + stage);

                    BorderPane bp = (BorderPane) scene.getRoot();
                    HBox hb = (HBox) bp.getCenter();
                    System.out.println("hb  " + hb);
                    TabPane tp = (TabPane) hb.getChildren().get(0);

                    System.out.println("tp  " + tp);

                    Tab downloads = tp.getTabs().get(5);
                    //  Tab downloads = (Tab)scene.lookup("#history");
                   

                    System.out.println("downloads  " + downloads);
                    SingleSelectionModel<Tab> selectionModel = tp.getSelectionModel();
                    SessionDownload sd = new SessionDownload(stage);

                    SlotTree st = sd.slotTree;
                    CustomTextField ctf = st.ctf;
                    ctf.setText(sessionId);

                    Button search = st.search;
                    search.fire();

                    downloads.setContent(sd);

                    selectionModel.select(downloads);

                    stage.setScene(scene);
                    stage.show();

                }
            });

            final ContextMenu contextMenu = new ContextMenu(downloadSession);
            UrlUtils.setCellFactory(tree, contextMenu);

        } catch (Exception e) {
            e.printStackTrace();

        }

    }

    public void processAsset(TreeView<String> tree) {
        MenuItem previewAsset = new MenuItem("Preview");
        previewAsset.setOnAction(new EventHandler<ActionEvent>() {
            public void handle(ActionEvent e) {
                mdPane.setDetailNode(null);
                final WebView browser = new WebView();
                final WebEngine webEngine = browser.getEngine();
                // 00002.mp4 (1088 MB) |16891|73302
                String sesId = UrlUtils.getSessionIdFromParser(selectedAsset.getValue());
                System.out.println("sesId = " + sesId);

                String assetId = UrlUtils.getAssetIdFromParser(selectedAsset.getValue());
                System.out.println("assetId = " + assetId);

                //webEngine.load("https://nyu.databrary.org/slot/6256/asset/9828");
                String url = UrlUtils.urlBase + "slot/" + sesId + "/asset/" + assetId;
                webEngine.load(url);
                mdPane.setDetailNode(browser);

                System.out.println("About");
            }
        });
        final ContextMenu contextMenu = new ContextMenu(previewAsset);
        UrlUtils.setCellFactory(tree, contextMenu);

    }

}

/*
treeView.setCellFactory(new Callback<TreeView<Tree>, TreeCell<Tree>>()
        {
            @Override
            public TreeCell<Tree> call(TreeView<Tree> treeView)
            {
                final TreeCell<Tree> cell = new TreeCell<Tree>()
                {
                    @Override
                    protected void updateItem(Tree item, boolean empty)
                    {
                        super.updateItem(item, empty);
                        if (!empty)
                        {
                            setText(item != null ? item.toString() : "");
                            setGraphic(createImageView(item));
                            setContextMenu(createContextMenuTreeItem(item));
                        }
                        else
                        {
                            setText(null);
                            setGraphic(null);
                            setContextMenu(null);
                        }
                    }
                };

                return cell;
            }
        });

 */
