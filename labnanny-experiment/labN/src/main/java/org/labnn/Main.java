/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

//http://stage.databrary.org:443/api/volume/1



package org.labnn;

import org.entity.Volume;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.annotation.JsonView;
import com.fasterxml.jackson.databind.DeserializationFeature;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import javafx.animation.FadeTransition;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.*;
import javafx.concurrent.*;
import javafx.geometry.*;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.effect.DropShadow;
import javafx.scene.image.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.stage.*;
import javafx.util.Duration;


public class Main extends Application {

    
    private Pane splashLayout;
    private ProgressBar loadProgress;
    private Label progressText;
    private Stage mainStage;
    private static final int SCENE_WIDTH = 1200;
    private static final int SCENE_HEIGHT = 600;
    Scene scene;

    //Tab pane
     BorderPane borderPane = new BorderPane();
    final TabPane tabPane = new TabPane();
    final Tab tab1 = new Tab("Volume Info");

    final Tab tab2 = new Tab("Staff Management");
    final Tab tab3 = new Tab("Scheduling module");
    final Tab tab4 = new Tab("Session/Visit Module");
    final Tab tab5 = new Tab("Data Provenance/Sharing");
    final Tab tab6 = new Tab("Publication/Presentation Management");
    final Tab tab7 = new Tab("Reporting Module");
    final Tab tab8 = new Tab("");

    Text volumeLabelT = new Text("++++++++++++++++++++++++++++++++++++++++++++++++");
    StringProperty jsonPretty = new SimpleStringProperty("");

    public static void main(String[] args) throws Exception {
        launch(args);
    }

    @Override
    public void init() {
       
        
        ClassLoader classLoader = getClass().getClassLoader();
        String imageUrl = classLoader.getResource("images/databrary.PNG").toExternalForm();
         ImageView splash = new ImageView(new Image(imageUrl));;

        //splash.setFitWidth(SPLASH_WIDTH/2);
        //splash.setFitHeight(SPLASH_HEIGHT/2);
        loadProgress = new ProgressBar();
        loadProgress.setPrefWidth(splash.getImage().getWidth());
        progressText = new Label("Will find friends for peanuts . . .");
        splashLayout = new VBox();
        splashLayout.getChildren().addAll(splash, loadProgress, progressText);
        progressText.setAlignment(Pos.CENTER);
        splashLayout.setStyle(
                "-fx-padding: 5; "
                + "-fx-background-color: cornsilk; "
                + "-fx-border-width:5; "
                + "-fx-border-color: "
                + "linear-gradient("
                + "to bottom, "
                + "chocolate, "
                + "derive(chocolate, 50%)"
                + ");"
        );
        splashLayout.setEffect(new DropShadow());
    }

    @Override
    public void start(final Stage initStage) throws Exception {
        
         volumeLabelT.textProperty().bind(jsonPretty);
        
        
       
        
        final Task<ObservableList<String>> friendTask = new Task<ObservableList<String>>() {
            @Override
            protected ObservableList<String> call() throws InterruptedException {
                ObservableList<String> foundVolumes
                        = FXCollections.<String>observableArrayList();
                ObservableList<String> availableFriends
                        = FXCollections.observableArrayList(
                                "Volume 1", "Volume 2", "Volume 3", "Volume 4", "Volume 5",
                                "Volume 6", "Volume 17", "Volume 8", "Volume 1",
                                "Volume 1", "Volume 1", "Volume 1", "Volume 1"
                        );

                updateMessage("Finding volumes . . .");
                for (int i = 0; i < availableFriends.size(); i++) {
                    Thread.sleep(150);
                    updateProgress(i + 1, availableFriends.size());
                    String nextFriend = availableFriends.get(i);
                    foundVolumes.add(nextFriend);
                    // updateMessage("Loading . . . found " + nextFriend);
                    updateMessage("Loading . . . ");
                }
                Thread.sleep(120);
                updateMessage("All volumes found.");

                return foundVolumes;
            }
        };

        showPreloader(
                initStage,
                friendTask,
                () -> showMainStage(friendTask.valueProperty())
        );
        new Thread(friendTask).start();
    }

    private void showMainStage(
            ReadOnlyObjectProperty<ObservableList<String>> friends
    ) {
       
        
        mainStage = new Stage(StageStyle.DECORATED);
        mainStage.setTitle("Databrary desktop");

        

        borderPane.setCenter(new Label("Hello databrary"));

        //peopleView.itemsProperty().bind(friends);
        VBox top = new VBox(2);
        top.getChildren().addAll(getMenuBar());

        borderPane.setTop(top);

        borderPane.setCenter(addHBox());

        final ListView<String> peopleView = new ListView<>();
        peopleView.itemsProperty().bind(friends);
       

        //Scene scene = new Scene(peopleView);
        scene = new Scene(borderPane, SCENE_WIDTH, SCENE_HEIGHT);
        tabPane.prefWidthProperty().bind(scene.widthProperty().subtract(3.0));
        
        //tabPane.getStyleClass().add("vTabPane");
      // Button  l =  new Button("Helllllo");
       
         tab1.setContent(volumeLabelT);
        
        ClassLoader classLoader = getClass().getClassLoader();
        
        String css = classLoader.getResource("styles/styles.css").toExternalForm();
        
        scene.getStylesheets().add(css);
        mainStage.setScene(scene);
        mainStage.show();
        
        scene.widthProperty().addListener((obs, oldScene, newScene) -> {
    if (newScene == null) {
        // not showing...
    } else {
        volumeLabelT.setWrappingWidth((double)newScene - 30);
    }
});
        
        // Platform.runLater(() -> jsonToJava());

        Platform.runLater(new Runnable() {
            public void run() {
                jsonToJava();
             //   tab4.setContent(new Label(volumeProperty.get()));
           
            }
        });

    }

    private void showPreloader(
            final Stage initStage,
            Task<?> task,
            InitCompletionHandler initCompletionHandler
    ) {
        progressText.textProperty().bind(task.messageProperty());
        loadProgress.progressProperty().bind(task.progressProperty());
        task.stateProperty().addListener((observableValue, oldState, newState) -> {
            if (newState == Worker.State.SUCCEEDED) {
                loadProgress.progressProperty().unbind();
                loadProgress.setProgress(1);
                initStage.toFront();
                FadeTransition fadeSplash = new FadeTransition(Duration.seconds(1.2), splashLayout);
                fadeSplash.setFromValue(1.0);
                fadeSplash.setToValue(0.0);
                fadeSplash.setOnFinished(actionEvent -> initStage.hide());
                fadeSplash.play();

                initCompletionHandler.complete();
            } // todo add code to gracefully handle other task states.
        });

        Scene splashScene = new Scene(splashLayout, Color.TRANSPARENT);
        final Rectangle2D bounds = Screen.getPrimary().getBounds();
        initStage.setScene(splashScene);
        //  initStage.setX(bounds.getMinX() + bounds.getWidth() / 2 - SPLASH_WIDTH / 2);
        //  initStage.setY(bounds.getMinY() + bounds.getHeight() / 2 - SPLASH_HEIGHT / 2);
        initStage.initStyle(StageStyle.TRANSPARENT);
        initStage.setAlwaysOnTop(true);
        initStage.show();
    }

    public interface InitCompletionHandler {

        void complete();
    }

    MenuBar getMenuBar() {
        final Menu menu1 = new Menu("File");
        final Menu menu2 = new Menu("Options");
        final Menu menu3 = new Menu("Help");

        MenuBar menuBar = new MenuBar();
        menuBar.getMenus().addAll(menu1, menu2, menu3);
        return menuBar;

    }

    public HBox addHBox() {
        HBox hbox = new HBox();
        // hbox.setPadding(new Insets(15, 12, 15, 12));
        hbox.setPadding(new Insets(2, 2, 2, 2));
        hbox.setSpacing(10);
        // hbox.setStyle("-fx-background-color: #336699;");
        //  hbox.setStyle("-fx-background-color: lightgray;");

        // Button buttonCurrent = new Button("Current");
        // buttonCurrent.setPrefSize(100, 20);
        // Button buttonProjected = new Button("Projected");
        // buttonProjected.setPrefSize(100, 20);
       
        hbox.getChildren().addAll(
                //   buttonCurrent,
                //buttonProjected,
                //  labNanny ,
                getTabPane()
        );

        return hbox;
    }

    public TabPane getTabPane() {
        // BorderPane borderPane = new BorderPane();
        // tabPane.setPrefSize(400, 400);

        tabPane.setSide(Side.TOP);
        tabPane.setTabClosingPolicy(TabPane.TabClosingPolicy.UNAVAILABLE);

        //  tabPane.setRotateGraphic(true);
        tabPane.setTabMinHeight(30);

//tabPane.setTabMaxHeight(30);
        // tab1.setText("From Databrary");
        //  tab2.setText("To Databrary");
        //  tab3.setText("Tab 3");
        //  tab4.setText("Tab 4");
        tabPane.getTabs().addAll(tab1, tab2, tab3, tab4, tab5, tab6, tab7);

        return tabPane;
        //  borderPane.setCenter(tabPane);
        // getChildren().add(borderPane);
    }

    public void jsonToJava() {
        System.out.println("staf1 --> ");
        ObjectMapper mapper = new ObjectMapper();
       
        System.out.println("staf2 --> ");

        try {
             mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            // Convert JSON string from file to Object
            //Staf staff = mapper.readValue(new File("C:\\staf.json"), Volume.class);
             Volume staff = mapper.readValue(new URL("http://stage.databrary.org:443/api/volume/1"), Volume.class);
            System.out.println("staf3 --> " + staff);
/*
            // Convert JSON string to Object
            String jsonInString = "{\"id\":\"1\"}";
            Volume staff4 = mapper.readValue(jsonInString, Volume.class);
            System.out.println("staf4--> " + staff4);
*/
            //Pretty print
            jsonPretty.setValue(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(staff).toString());
            System.out.println(volumeLabelT.getText());

        } catch (Exception e) {
               e.printStackTrace();
        }
    }
}
