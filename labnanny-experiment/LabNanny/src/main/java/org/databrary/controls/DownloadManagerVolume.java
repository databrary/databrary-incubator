/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.databrary.controls;

import java.awt.Desktop;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.EnumSet;
import javafx.application.Application;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import org.databrary.utils.UrlUtils;

public class DownloadManagerVolume extends BorderPane {

    //static Task<Boolean> copyWorker;
    //  static ArrayList<Task<Boolean>> copyWorker;
    static Task<Boolean> copyWorker;

    // final int numFiles = 30;
    // final int numFiles = 1;
    private ExecutorService threadPool;

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Application.launch(args);
    }

    public void stopThreadpool() throws Exception {
        //  super.stop();
        threadPool.shutdown();
    }

    public DownloadManagerVolume(final String volumeId) {
        // st.checkTreeView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        System.out.println("dir " + getDir());

        threadPool = Executors.newFixedThreadPool(1);

        // final HBox vb = new HBox();
        final Label label = new Label("Download progress:");
        final ProgressBar progressBar = new ProgressBar(0);
        final ProgressIndicator progressIndicator = new ProgressIndicator(0);

        final HBox hb = new HBox();
        hb.setSpacing(5);
        hb.setAlignment(Pos.CENTER);

        final TextField textArea = new TextField();
        textArea.requestFocus();
        textArea.setEditable(false);
        textArea.setPrefWidth(400);
        hb.getChildren().addAll(textArea, label, progressBar, progressIndicator);
        //mainPane.setTop(hb);
        setTop(hb);

        final Button startButton = new Button("Start");
        final Button cancelButton = new Button("Cancel");

        //textArea.setPrefSize(400, 70);
        final HBox hb2 = new HBox();
        hb2.setPadding(new Insets(4, 4, 4, 4));
        hb2.setSpacing(5);
        hb2.setAlignment(Pos.CENTER);
        hb2.getChildren().addAll(startButton, cancelButton);
        //mainPane.setBottom(hb2);
        setBottom(hb2);

        // wire up start button
        startButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {

                try {
                    openFolder(new File(getDir()));
                } catch (Exception e) {
                    e.printStackTrace();
                }

                startButton.setDisable(true);

                String urls =   UrlUtils.urlBase  + "volume/" + volumeId + "/zip/false";
                String fileOutputs = getDir() + File.separator + volumeId + ".zip";

                System.out.println("-------------------------------the shakir sizeurl ");
                progressBar.setProgress(0);
                progressIndicator.setProgress(0);
                textArea.setText("");
                cancelButton.setDisable(false);
                
              

                copyWorker = createWorker(urls, fileOutputs);
                copyWorker.setOnSucceeded(e -> {

                    startButton.setDisable(false);
                    cancelButton.setDisable(true);

                    progressBar.progressProperty().unbind();
                    progressBar.setProgress(0);
                    progressIndicator.progressProperty().unbind();
                    progressIndicator.setProgress(0);
                    textArea.setText("File transfer completed.");
                    
                    try {
                    stopThreadpool();
                    }
                    
                    catch(Exception ex) {
                        ex.printStackTrace();
                    
                    }
                    
                    
                });

                progressBar.progressProperty().unbind();
                progressBar.progressProperty().bind(copyWorker.progressProperty());
                progressIndicator.progressProperty().unbind();
                progressIndicator.progressProperty().bind(copyWorker.progressProperty());

                //   int sel = st.getCheckedIndex(i);
                copyWorker.messageProperty().addListener((ObservableValue<? extends String> observable, String oldValue, String newValue)
                        -> textArea.setText(newValue + "\n")); //    st.checkTreeView.getSelectionModel().select(sel);

                // st.checkTreeView.getSelectionModel().select(i);
                threadPool.submit(copyWorker);

            }
        }
        );

        // cancel button will kill worker and reset.
        cancelButton.setOnAction(
                (ActionEvent event) -> {
                    startButton.setDisable(false);
                    cancelButton.setDisable(true);

                    copyWorker.cancel(true);
                    progressBar.progressProperty().unbind();
                    progressBar.setProgress(0);
                    progressIndicator.progressProperty().unbind();
                    progressIndicator.setProgress(0);
                    textArea.setText("File transfer was cancelled.");
                    //  st.rootTree.setIndeterminate(true);
                }
        );
        cancelButton.setDisable(true);

        // primaryStage.setScene(scene);
        //  primaryStage.show();
    }

    // private Task<Boolean> createWorker(String urlSt, String fileName) {
    private Task<Boolean> createWorker(String urls, String fileName) {
        return new Task<Boolean>() {

            @Override
            protected Boolean call() throws Exception {

                LocalDateTime start = LocalDateTime.now();     // The current date and time
                System.out.println("DownloadFileChannel" + start);
                File file = new File(fileName);
                URL url = new URL(urls);

                try {

                    while (true) {

                        long transferedSize = file.length();
                        System.out.println("transferedSize = " + transferedSize);

                        System.out.println("Transfered size = " + file.length());

                        URLConnection connection = url.openConnection();

                        connection.setRequestProperty("Range", "bytes=" + transferedSize + "-");
                        ReadableByteChannel rbc = Channels.newChannel(connection.getInputStream());

                        long contentLength = 0;
                        contentLength = connection.getContentLengthLong();

                        System.out.println("contentLength size = " + contentLength);

                        if (transferedSize > 0 && contentLength == 0) {
                            updateMessage("Completed.");
                            updateProgress(0, 1);
                            System.out.println("File is complete");

                            // return true;
                            break;
                        }

                        long remainingSize = contentLength;
                        if (remainingSize <= 0) {
                            // return true;
                            break;
                        }

                        System.out.println("remainingSize size = " + remainingSize);

                        long buffer = remainingSize;
                        if (remainingSize > 65536) {
                            buffer = 1 << 16;
                        }

                        System.out.println("Remaining size: " + remainingSize);
                        FileOutputStream fos = new FileOutputStream(file, true);

                        if (transferedSize == remainingSize) {
                            System.out.println("File is complete");

                            rbc.close();
                            fos.close();
                            fos.flush();
                            return true;
                        }

                        if (isCancelled()) {
                            updateMessage("File transfer was cancelled.");
                            break;
                        }

                        System.out.println("Continue downloading at " + transferedSize);
                        while (remainingSize > 0) {
                            long delta = fos.getChannel().transferFrom(rbc, transferedSize, buffer);
                            transferedSize += delta;
                            // System.out.println(transferedSize + " bytes received");

                            updateMessage(transferedSize / (1024 * 1024) + " " + "MB received " + fileName);
                            updateProgress(transferedSize, contentLength); // (progress, max)
                            //  updateProgress(transferedSize, max); 
                            if (delta == 0) {
                                break;
                            }

                            if (isCancelled()) {
                                updateMessage("File transfer was cancelled.");
                                break;
                            }

                        }

                        rbc.close();
                        fos.close();
                        fos.flush();
                        System.out.println("Download incomplete, retrying");

                        LocalDateTime end = LocalDateTime.now();     // The current date and time
                        System.out.println("DownloadFileChannel" + end);
                        System.out.println(" transferedSiz " + transferedSize);
                        System.out.println("contentLength " + contentLength);

                        if (transferedSize == contentLength) {
                            break;
                            // return false;
                        } else {
                            return true;
                        }

                    }

                } catch (Exception e) {
                    e.printStackTrace();

                }

                return true;
            }
        };
    }

    public String getDir() {
        String dir = System.getProperty("user.home") + File.separatorChar + "databrary";
        File file = new File(dir);
        if (!file.exists()) {
            file.mkdir();
        }
        return dir;
    }

    public static void openFolder(File folder) throws Exception {
        if (Desktop.isDesktopSupported()) {
            Desktop.getDesktop().open(folder);
        }
    }

    public String getFileExtension(String url) {
        String extension = url.substring(url.lastIndexOf("."));
        return extension;

    }

}
