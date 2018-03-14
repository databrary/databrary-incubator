/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.entity;

/**
 *
 * @author Ella
 */
public class Volume {

    public String id;
    public String name;
    public String body;
    public String doi;
    public String creation;
    
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }
    
    
    public String getName() {
        return name;
    }
 
    public void setName(String name) {
        this.name = name;
    }
    
    public String getBody() {
        return body;
    }
 
    public void setBody(String body) {
        this.body = body;
    }
    
     public String getDoi() {
        return doi;
    }
 
    public void setDoi(String doi) {
        this.doi = doi;
    }
    
     public String getCreation() {
        return creation;
    }
 
    public void setCreation(String creation) {
        this.creation = creation;
    }
    

}
