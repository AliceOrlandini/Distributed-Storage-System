package com.unipi.application.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.client.MultipartBodyBuilder;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import java.io.IOException;
import java.io.InputStream;

@Service
public class UploadFileService {

    private String backendUrl = "http://localhost:5000";

    private static final Logger LOGGER = LoggerFactory.getLogger(UploadFileService.class);

    public boolean uploadFile(InputStream inputStream, String fileName) throws IOException {
        try {
            byte[] fileBytes = inputStream.readAllBytes();

            MultipartBodyBuilder builder = new MultipartBodyBuilder();
            builder.part("file", fileBytes).filename(fileName);

            LOGGER.info("Uploading file: {} to {}", fileName, backendUrl);
            String response = WebClient.create(backendUrl)
                    .post()
                    .uri("/upload")
                    .contentType(MediaType.MULTIPART_FORM_DATA)
                    .bodyValue(builder.build())
                    .retrieve()
                    .bodyToMono(String.class)
                    .block();

            // if the response status code is 204 No Content, the file was uploaded successfully
            return response == null || response.isEmpty();
        } catch(IOException e) {
            throw new IOException("Error uploading file", e);
        }
    }
}
