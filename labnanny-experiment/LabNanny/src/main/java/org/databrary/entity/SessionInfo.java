/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.databrary.entity;

/**
 *
 * @author Shakir
 */
public class SessionInfo {

    public String id;
    public String name;
    public String date;
    public String release;

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

    
    public String getDate() {
        return date;
    }

    public void setCreation(String date) {
        this.date = date;
    }
    
    
    public String getRelease() {
        return release;
    }

    public void setRelease(String release) {
        this.release = release;
    }
    
  
}
