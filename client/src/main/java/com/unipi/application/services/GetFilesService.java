package com.unipi.application.services;

import java.util.Collections;
import java.util.List;
import com.unipi.application.model.FileDetails;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import reactor.core.publisher.Mono;

@Service
public class GetFilesService {

    private static final Logger LOGGER = LoggerFactory.getLogger(GetFilesService.class);

    public List<FileDetails> getFiles(String jwtToken) {
        try {
            String backendUrl = "http://localhost:8080";
            LOGGER.info("Fetching file list from {}", backendUrl);

            FilesResponse getFilesResponse = WebClient.create(backendUrl)
                    .get()
                    .uri("/files")
                    .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                    .exchangeToMono(response -> {
                        if (response.statusCode().isError()) {
                            LOGGER.error("Error: {}", response.statusCode());
                            return Mono.just(new FilesResponse());
                        } else {
                            return response.bodyToMono(FilesResponse.class);
                        }
                    })
                    .block();

            if(getFilesResponse == null || getFilesResponse.getFiles() == null || getFilesResponse.getFiles().isEmpty()) {
                LOGGER.error("Error fetching file list");
                return Collections.emptyList();
            }
            return getFilesResponse.getFiles();
        } catch (Exception e) {
            LOGGER.error("Error fetching file list", e);
            throw new RuntimeException("Error fetching file list", e);
        }
    }

    public static class FilesResponse {
        private List<FileDetails> files;

        public List<FileDetails> getFiles() {
            return files;
        }

        public void setFiles(List<FileDetails> files) {
            this.files = files;
        }
    }

}
