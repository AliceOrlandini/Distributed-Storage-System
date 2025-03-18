package com.unipi.application.model;

public class FilePositionModel {

    private String ip;
    private String chunkName;
    private int chunkPosition;

    public FilePositionModel() {
    }

    public FilePositionModel(String ip, String chunkHash, int chunkPosition) {
        this.ip = ip;
        this.chunkName = chunkHash;
        this.chunkPosition = chunkPosition;
    }

    public String getIp() {
        return ip;
    }

    public void setIp(String ip) {
        this.ip = ip;
    }

    public String getChunkName() {
        return chunkName;
    }

    public void setChunkName(String chunkName) {
        this.chunkName = chunkName;
    }

    public int getChunkPosition() {
        return chunkPosition;
    }

    public void setChunkPosition(int chunkPosition) {
        this.chunkPosition = chunkPosition;
    }

}
