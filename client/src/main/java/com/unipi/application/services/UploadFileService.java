package com.unipi.application.services;

import java.io.IOException;
import java.io.InputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.MultipartBodyBuilder;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;
import com.unipi.application.config.Connection;

@Service
public class UploadFileService {

    private static final Logger LOGGER = LoggerFactory.getLogger(UploadFileService.class);

    public boolean uploadFile(InputStream inputStream, String fileName, String jwtToken) throws IOException {
        try {
            byte[] fileBytes = inputStream.readAllBytes();

            MultipartBodyBuilder builder = new MultipartBodyBuilder();
            builder.part(fileName, fileBytes).filename(fileName);

            LOGGER.info("Uploading file: {} to {}", fileName, Connection.BACKEND_URL);

            String uploadFileResponse = WebClient.create(Connection.BACKEND_URL)
                .post()
                .uri("/upload")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                .contentType(MediaType.MULTIPART_FORM_DATA)
                .bodyValue(builder.build())
                .exchangeToMono(response -> {
                    if (response.statusCode().isError()) {
                        LOGGER.error("Error: {}", response.statusCode());
                        return Mono.just("error");
                    } else {
                        return response.bodyToMono(String.class);
                    }
                })
                .block();
            if (uploadFileResponse != null && uploadFileResponse.equals("error")) {
                LOGGER.error("Upload failed for file: {}", fileName);
                return false;
            }
            // if the response status code is 204 No Content, the file was uploaded successfully
            return true;
        } catch(IOException e) {
            throw new IOException("Error uploading file", e);
        }
    }

}
