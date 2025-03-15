package com.unipi.application.model;

public class Chunck {

    private int position;
    private byte[] data;

    public Chunck() {
    }

    public Chunck(int position, byte[] data) {
        this.position = position;
        this.data = data;
    }

    public int getPosition() {
        return position;
    }

    public void setPosition(int position) {
        this.position = position;
    }

    public byte[] getData() {
        return data;
    }

    public void setData(byte[] data) {
        this.data = data;
    }

    public void setPositionFromFilePosition(FilePosition filePosition) {
        this.position = filePosition.getChunkPosition();
    }

    @Override
    public String toString() {
        return "Chunck{" +
                "position=" + position +
                ", data=" + (data != null ? data.length + " bytes" : "null") +
                '}';
    }
}
