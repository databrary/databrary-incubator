/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.databrary.utils;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.ListView;
import java.util.regex.Pattern;
import javafx.concurrent.Worker;
import javafx.concurrent.Worker.State;
import javafx.scene.control.CheckBoxTreeItem;
import javafx.scene.control.ScrollPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import org.databrary.entity.Volume;


/**
 *
 * @author Shakir
 */
public class UrlUtils {

    public static String fileName = System.getProperty("user.home") + File.separatorChar + "dbrary";
   // public static String urlBase = "https://nyu.databrary.org/";
   // public static String urlBaseApi = "https://nyu.databrary.org/api/";
   
     public static String urlBase = "http://stage.databrary.org:443/";
     public static String urlBaseApi = "http://stage.databrary.org:443/api/";

    public static String constants = "http://stage.databrary.org:443/api/constants";  //{"-800":{"id":-800,"mimetype":"video/mp4","extension":"mp4","name":"MPEG-4 video","transcodable":-800},

   

    public static String getExtension(String format) {

        Map<String, String> map = new HashMap<>();
        map.put("-800", "mp4");
        map.put("-700", "jpg");
        map.put("-600", "mp3");
        map.put("1", "txt");
        map.put("2", "csv");
        //  map.put("3", "ooo");  reserved
        map.put("4", "rtf");
        map.put("5", "png");
        map.put("6", "pdf");
        map.put("7", "doc");
        map.put("8", "odf");
        map.put("9", "docx");
        map.put("10", "xsl");
        map.put("11", "ods");
        map.put("12", "xlsx");
        map.put("13", "ppt");
        map.put("14", "odp");
        map.put("15", "pptx");
        map.put("16", "opf");
        map.put("18", "webm");
        map.put("19", "mpg");
        map.put("20", "mov");
        map.put("21", "mts");
        map.put("22", "avi");
        map.put("23", "sav");
        map.put("24", "wav");
        map.put("25", "wmv");
        map.put("26", "cha");
        map.put("27", "aac");
        map.put("28", "wma");
        map.put("29", "its");
        map.put("30", "dv");
        map.put("31", "eaf");

        if (map.containsKey(format)) {
            return map.get(format);

        } else {
            return "";
        }

    }

    
   // search al volumes that contains word "facial" 
    public static ArrayList<String> getVolumesBySearch(String key_Word) throws Exception {
        //sample  https://nyu.databrary.org/api/search?q=facial     search al volumes that contains word "facial" 
        String encoded_volume_name = key_Word.replaceAll(" ", "%20");
        String url = urlBaseApi + "search?q=" + encoded_volume_name;
        ArrayList<String> names = new ArrayList();

        final ObjectMapper mapper = new ObjectMapper();
        JsonNode json = mapper.readTree(new URL(url));

        // final JsonNode json = mapper.readTree(file);
        final JsonNode arrNode = json.get("response").get("docs");
        if (arrNode.isArray()) {
            for (final JsonNode objNode : arrNode) {
                //System.out.println(objNode.get("id").asText() + ": " + objNode.get("name").asText());
                names.add(objNode.get("id").asText() + ": "
                        + objNode.get("name").asText());
            }
        }

        return names;

    }

    public static String getVolumeNameById(String volume_id) throws Exception {
        //sample https://nyu.databrary.org/api/volume/1 
        String url = urlBaseApi + "volume/" + volume_id;
        final ObjectMapper mapper = new ObjectMapper();

        // JsonNode json;
        try {
            JsonNode json = mapper.readTree(new URL(url));
            final JsonNode name = json.get("name");
            System.out.println(name.asText() + "^" + volume_id.trim());
            return name.asText() + "^" + volume_id.trim();
        } catch (Exception e) {
            return "";

        }

    }

    public static Volume getVolumeObjectById(String volume_id) throws Exception {
        //sample https://nyu.databrary.org/api/volume/1 
        String url = urlBaseApi + "volume/" + volume_id;
        final ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        try {

            Volume volume = mapper.readValue(new URL(url), Volume.class);
            return volume;
        } catch (Exception e) {
            return null;

        }

    }

    //consider a better solution 
    public static String getVolumeIdByName(String volume_name) throws Exception {

        try {

            File file = new File(fileName);
            FileReader fileReader = new FileReader(file);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            String line;
            while ((line = bufferedReader.readLine()) != null) {

                String[] key_value = line.split("\\^");
                String volumeName = key_value[0];
                String volumeId = key_value[1];
                if (volume_name.equals(volumeName)) {
                    return volumeId;
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
        return "";
    }

    /*
    public static String getVolumeIdByName(String volume_name) throws Exception {
        //This method is for internal use by application. user shoudnet enter it in 
        //the search box. it shoud return only 1 id if there is any. 
        //sample  https://nyu.databrary.org/api/search?q=volume_name     search al volumes that contains volume_name 
        String encoded_volume_name = volume_name.replaceAll(" ", "%20");
        String url = urlBaseApi + "search?q=" + encoded_volume_name;

        final ObjectMapper mapper = new ObjectMapper();
        JsonNode json = mapper.readTree(new URL(url));

        final JsonNode arrNode = json.get("response").get("docs");
        String id = "";
        if (arrNode.isArray()) {
            for (final JsonNode objNode : arrNode) {
                //System.out.println(objNode.get("id").asText() + ": " + objNode.get("name").asText());
                id = objNode.get("id").asText();
            }
        }

        return id;

    }

     */
    public static ArrayList<String> getVolumeContainersIds(String volume_id) throws Exception {
        //sample  http://stage.databrary.org:443/api/volume/1?top&containers   containers (sessions) in volume 1 
        String url = urlBaseApi + "volume/" + volume_id + "?top&containers";
        ArrayList<String> containers = new ArrayList();
        try {
            final ObjectMapper mapper = new ObjectMapper();
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            JsonNode json = mapper.readTree(new URL(url));

            // final JsonNode json = mapper.readTree(file);
            final JsonNode arrNode = json.get("containers");
            if (arrNode.isArray()) {
                for (final JsonNode objNode : arrNode) {
                    //  System.out.println("name " + objNode.get("name"));
                    //System.out.println(objNode.get("id").asText() + ": " + objNode.get("name").asText());
                    if (objNode.get("name") != null) {
                        containers.add(objNode.get("id").asText());
                    }
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        return containers;      //return [6256, 6257, 6540 .....] for a volumw 1

    }

    //session or slots  
    //session or slots  
    public static ArrayList<String> getVolumeContainersNames(String volume_id) throws Exception {
        //sample  http://stage.databrary.org:443/api/volume/1?top&containers   containers (sessions) in volume 1 
        String url = urlBaseApi + "volume/" + volume_id + "?top&containers";
        ArrayList<String> containers = new ArrayList();

        final ObjectMapper mapper = new ObjectMapper();
        try {
            JsonNode json = mapper.readTree(new URL(url));

            // final JsonNode json = mapper.readTree(file);
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            final JsonNode arrNode = json.get("containers");
            if (arrNode.isArray()) {
                for (final JsonNode objNode : arrNode) {
                    //System.out.println(objNode.get("id").asText() + ": " + objNode.get("name").asText());
                    if (objNode.get("id") != null && objNode.get("name") != null && objNode.get("date") != null) {
                        containers.add(objNode.get("name").asText() + " " + objNode.get("date").asText() + " " + objNode.get("id").asText());
                    } else {
                        if (objNode.get("id") != null && objNode.get("name") != null) {
                            containers.add(objNode.get("name").asText() + " " + objNode.get("id").asText());

                        }

                    }

                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            return containers;

        }

        return containers;      //return  [Advisory Board Meeting 2013-10-28, Advisory Board Meeting 2014-10-17] for volume 1

    }

    public static Map<String, String> getVolumeContainersNamesAndIds(String volume_id) throws Exception {
        //sample  http://stage.databrary.org:443/api/volume/1?top&containers   containers (sessions) in volume 1 
        String url = urlBaseApi + "volume/" + volume_id + "?top&containers";
        Map<String, String> containers = new HashMap();

        final ObjectMapper mapper = new ObjectMapper();
        try {
            JsonNode json = mapper.readTree(new URL(url));

            // final JsonNode json = mapper.readTree(file);
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
            final JsonNode arrNode = json.get("containers");
            if (arrNode.isArray()) {
                for (final JsonNode objNode : arrNode) {
                    //System.out.println(objNode.get("id").asText() + ": " + objNode.get("name").asText());
                    if (objNode.get("id") != null && objNode.get("name") != null && objNode.get("date") != null) {
                        containers.put(objNode.get("id").asText(), objNode.get("name").asText() + " " + objNode.get("date").asText() + " " + objNode.get("id").asText());
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            return containers;

        }

        return containers;      //return  [Advisory Board Meeting 2013-10-28, Advisory Board Meeting 2014-10-17] for volume 1

    }

    public static ArrayList<String> getFileNamesInSession(String slot_id) throws Exception {
        //sample             http://stage.databrary.org:443/api/slot/6256/-?assets    6256 is a session_id or slot_id or container_id. All are the same. 
        //top level          http://stage.databrary.org:443/api/slot/14447/-?assets 
        
        String url = urlBaseApi + "slot/" + slot_id + "/-?assets";
        ArrayList<String> files = new ArrayList();

        final ObjectMapper mapper = new ObjectMapper();
        try {
            JsonNode rootNode = mapper.readTree(new URL(url));
            // final JsonNode json = mapper.readTree(file);
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

            JsonNode assts = rootNode.path("assets");
            Iterator<JsonNode> elements = assts.elements();
            while (elements.hasNext()) {
                JsonNode asset = elements.next();
                String size = "";
                if (asset.get("size") != null) {
                    size = "(" + Long.parseLong(asset.get("size").toString()) / (1024 * 1024) + " MB)";
                }
                String file_name = "";

                if (asset.get("format") != null && asset.get("name") != null) {
                    file_name = asset.get("name").asText() + "." + UrlUtils.getExtension(asset.get("format").asText()) + " " + size;
                    
                }
                files.add(file_name);

                //  System.out.println("file_name = " + file_name);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return null;

        }

        return files;      //return  [Advisory Board Meeting 2013-10-28, Advisory Board Meeting 2014-10-17] for volume 1

    }

    
    
     public static ArrayList<String> getFileNamesInSession_b(String slot_id) throws Exception {
        //sample             http://stage.databrary.org:443/api/slot/6256/-?assets    6256 is a session_id or slot_id or container_id. All are the same. 
        //top level          http://stage.databrary.org:443/api/slot/14447/-?assets 
        
        String url = urlBaseApi + "slot/" + slot_id + "/-?assets";
        ArrayList<String> files = new ArrayList();

        final ObjectMapper mapper = new ObjectMapper();
        try {
            JsonNode rootNode = mapper.readTree(new URL(url));
            // final JsonNode json = mapper.readTree(file);
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

            JsonNode assts = rootNode.path("assets");
            Iterator<JsonNode> elements = assts.elements();
            while (elements.hasNext()) {
                JsonNode asset = elements.next();
                String size = "";
                if (asset.get("size") != null) {
                    size = "(" + Long.parseLong(asset.get("size").toString()) / (1024 * 1024) + " MB)";
                }
                String file_name = "";

                if (asset.get("format") != null && asset.get("name") != null) {
                  file_name =   asset.get("id").asText().toString() + "~" + size + " " + asset.get("name").asText().toString() + "." + UrlUtils.getExtension(asset.get("format").asText().toString());  
                    
                  
                }
                files.add(file_name);

                //  System.out.println("file_name = " + file_name);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return null;

        }

        return files;      //return  [Advisory Board Meeting 2013-10-28, Advisory Board Meeting 2014-10-17] for volume 1

    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    public static void writeDatabraryVolumesNames() throws Exception {

        File file = new File(fileName);
        if (!file.exists()) {
            file.createNewFile();
        }

        FileWriter fw = new FileWriter(file.getAbsoluteFile());
        BufferedWriter bw = new BufferedWriter(fw);

        for (int i = 0; i < 50; i++) {
            try {
                String volumeName = UrlUtils.getVolumeNameById(i + "");
                Optional<String> opt = Optional.ofNullable(volumeName);
                opt.ifPresent(name -> {
                    try {
                        if (!name.isEmpty()) {
                            bw.write(name + System.lineSeparator());
                        }
                    } catch (IOException ex) {
                        Logger.getLogger(UrlUtils.class.getName()).log(Level.SEVERE, null, ex);
                    }
                });
            } catch (Exception e) {
                e.printStackTrace();
            }

        }

        bw.flush();
        bw.close();

    }

    public static Map<String, String> loadVolumesNamesFromFile() throws Exception {

        Map<String, String> containers = new HashMap<String, String>();

        try {

            File file = new File(fileName);
            FileReader fileReader = new FileReader(file);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            String line;
            while ((line = bufferedReader.readLine()) != null) {

                String[] key_value = line.split("\\^");
                String volumeName = key_value[0];
                String volumeId = key_value[1];
                containers.put(volumeName, volumeId);

            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        return containers;      //return  [Advisory Board Meeting 2013-10-28, Advisory Board Meeting 2014-10-17] for volume 1

    }
    
    
    
    
    
    
    public static WebView getBrowser() {
    
    final WebView browser = new WebView();
    final WebEngine webEngine = browser.getEngine();

   // ScrollPane scrollPane = new ScrollPane();
   // scrollPane.setContent(browser);
    

    webEngine.getLoadWorker().stateProperty()
        .addListener(new ChangeListener<State>() {
          @Override
          public void changed(ObservableValue ov, State oldState, State newState) {

            if (newState == Worker.State.SUCCEEDED) {
            //  stage.setTitle(webEngine.getLocation());
              System.out.println("Location = " + webEngine.getLocation());
            }

          }
        });
        webEngine.load("https://nyu.databrary.org/");

    return browser;
    }
    
    
    

    public static String getSelectedVolume(ListView<String> lv) {
        StringProperty selected = new SimpleStringProperty("");
        lv.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<String>() {
            @Override
            public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
                // Your action here
                if (newValue != null) {
                    selected.setValue(newValue);
                }

            }
        });

        return selected.getValue();

    }

}
