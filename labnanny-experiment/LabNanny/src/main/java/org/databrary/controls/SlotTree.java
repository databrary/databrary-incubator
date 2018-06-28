/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.databrary.controls;


import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import impl.org.controlsfx.i18n.Localization;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javafx.application.Application;
import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBoxTreeItem;
import javafx.scene.control.Label;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.controlsfx.control.CheckTreeView;
import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.control.textfield.TextFields;
import org.databrary.utils.UrlUtils;

/**
 *
 * @author gusaros
 */
public class SlotTree extends BorderPane {

    CheckBoxTreeItem<String> rootTree;
    CheckTreeView<String> checkTreeView;
    CustomTextField ctf;
    String slotId = "";
    String[] selectedAssetIdFullName; // = "";
    String[] assetId;
    String[] fileN; // = "";
    String[] ext; // = "";
    boolean[] isleaf;
    int size;
    
    public Button search;
    

    public SlotTree() {

        search = new Button("Search");
        search.requestFocus();

        ctf = (CustomTextField) TextFields.createClearableTextField();
        ctf.setPromptText("Enter Session ID");

        HBox hb = new HBox(10);
        hb.setPadding(new Insets(10, 10, 10, 10));
        hb.getChildren().addAll(ctf, search);

        VBox vb = new VBox(10);

        vb.getChildren().addAll(hb);

        search.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                // createTree();
                vb.getChildren().clear();
                // vb.getChildren().addAll(hb, createTree(), clear);
                vb.getChildren().addAll(hb, createTree());

            }
        });

        setCenter(vb);

    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        //    launch(args);
    }

    public CheckTreeView createTree() {

        rootTree = new CheckBoxTreeItem<String>("Slot " + ctf.getText());
        rootTree.setExpanded(true);
        try {
            ArrayList<String> adlar = UrlUtils.getFileNamesInSession_b(ctf.getText().trim());

            for (String ad : adlar) {
                CheckBoxTreeItem<String> item = new CheckBoxTreeItem<String>();
                item.setValue(ad);
                rootTree.getChildren().addAll(item);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        // Create the CheckTreeView with the data 
        checkTreeView = new CheckTreeView<>(rootTree);

        // and listen to the relevant events (e.g. when the checked items change).
        checkTreeView.getCheckModel().getCheckedItems().addListener(new ListChangeListener<TreeItem<String>>() {
            public void onChanged(ListChangeListener.Change<? extends TreeItem<String>> c) {
                size = checkTreeView.getCheckModel().getCheckedItems().size();

                if (size > 0) {
                    System.out.println("First checked " + getCheckedIndex(0));

                }

                if (size > 0) {
                    selectedAssetIdFullName = new String[size];
                    ext = new String[size];
                    fileN = new String[size];
                    assetId = new String[size];
                    isleaf = new boolean[size];

                    for (int i = 0; i < size; i++) {
                        if (checkTreeView.getCheckModel().getCheckedItems().get(i).isLeaf()) {

                            isleaf[i] = true;
                            selectedAssetIdFullName[i] = checkTreeView.getCheckModel().getCheckedItems().get(i).getValue();
                            ext[i] = selectedAssetIdFullName[i].split("\\.")[1];
                            // System.out.println("after tilda "  + selectedAssetIdFullName.split("\\.")[1] );
                            fileN[i] = selectedAssetIdFullName[i].split("\\)")[1];
                            assetId[i] = selectedAssetIdFullName[i].split("~")[0];

                            slotId = ctf.getText().trim();
                        } else {
                            isleaf[i] = false;
                        }
                        //  System.out.println(checkTreeView.getCheckModel().getCheckedItems());
                    }

                }
            }
        });

        return checkTreeView;

    }

    public void jsonToJava() {
        //Use as Example
        ObjectMapper mapper = new ObjectMapper();
        try {
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

            JsonNode rootNode = mapper.readTree(new URL("http://stage.databrary.org:443/api/slot/6256/-?assets"));
            JsonNode assts = rootNode.path("assets");

            Iterator<JsonNode> elements = assts.elements();
            while (elements.hasNext()) {
                JsonNode asset = elements.next();
                //   System.out.println("assets = " + asset.get("permission"));  veryimportant
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    int getCheckedIndex(int checked) {

        int size = checkTreeView.getCheckModel().getItemCount();

        System.out.println("size +++++++++++++ " + size);

        Map<Integer, Integer> map = new HashMap<>();
        int counter = 0;
        for (int i = 0; i < size; i++) {

            System.out.println(checkTreeView.getItemBooleanProperty(i).get());
            if (checkTreeView.getItemBooleanProperty(i).get()) {

                map.put(counter, i);
                counter++;
            }

        }
        return map.get(checked);

    }

}
