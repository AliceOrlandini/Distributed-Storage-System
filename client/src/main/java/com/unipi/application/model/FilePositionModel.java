package com.unipi.application.model;

public class FilePositionModel {

    private String ip;
    private String chunkHash;
    private int chunkPosition;

    public FilePositionModel() {
    }

    public FilePositionModel(String ip, String chunkHash, int chunkPosition) {
        this.ip = ip;
        this.chunkHash = chunkHash;
        this.chunkPosition = chunkPosition;
    }

    public String getIp() {
        return ip;
    }

    public void setIp(String ip) {
        this.ip = ip;
    }

    public String getChunkHash() {
        return chunkHash;
    }

    public void setChunkHash(String chunkHash) {
        this.chunkHash = chunkHash;
    }

    public int getChunkPosition() {
        return chunkPosition;
    }

    public void setChunkPosition(int chunkPosition) {
        this.chunkPosition = chunkPosition;
    }

}
