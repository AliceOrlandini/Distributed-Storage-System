package com.unipi.application.services;

import com.unipi.application.model.ChunckModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.ArrayList;
import com.unipi.application.config.Connection;

@Service
public class SaveFileService {

    private static final Logger LOGGER = LoggerFactory.getLogger(SaveFileService.class);

    private byte[] reassembleFile(List<ChunckModel> chunks) throws IOException {
        if (chunks == null || chunks.isEmpty()) {
            LOGGER.warn("No chunks found");
            return new byte[0];
        }

        List<ChunckModel> mutableChunks = new ArrayList<>(chunks);
        mutableChunks.sort(Comparator.comparingInt(ChunckModel::getPosition));

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        for (ChunckModel chunk : mutableChunks) {
            outputStream.write(chunk.getData());
        }

        LOGGER.info("File assembled successfully from {} chunk.", mutableChunks.size());
        return outputStream.toByteArray();
    }

    public boolean saveFile(List<ChunckModel> chunks, String filePath) throws IOException {
        byte[] fileData = reassembleFile(chunks);
        if (fileData.length == 0) {
            LOGGER.warn("The assembled file is empty, not saved");
            return false;
        }
        Path path = Paths.get(filePath);
        Files.write(path, fileData);
        LOGGER.info("File saved successfully in {}", filePath);
        return true;
    }
}
