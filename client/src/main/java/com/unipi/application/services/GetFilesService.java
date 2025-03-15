package com.unipi.application.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import java.util.Collections;
import java.util.List;

@Service
public class GetFilesService {

    private String backendUrl = "http://localhost:5000";
    private static final Logger LOGGER = LoggerFactory.getLogger(GetFilesService.class);

    public List<String> getFiles() {
        try {
            LOGGER.info("Fetching file list from {}", backendUrl);
            FilesResponse response = WebClient.create(backendUrl)
                    .get()
                    .uri("/files")
                    .retrieve()
                    .bodyToMono(FilesResponse.class)
                    .block();
            return (response != null && response.getFiles() != null)
                    ? response.getFiles()
                    : Collections.emptyList();
        } catch (Exception e) {
            LOGGER.error("Error fetching file list", e);
            throw new RuntimeException("Error fetching file list", e);
        }
    }

    public static class FilesResponse {
        private List<String> files;

        public List<String> getFiles() {
            return files;
        }

        public void setFiles(List<String> files) {
            this.files = files;
        }
    }
}
