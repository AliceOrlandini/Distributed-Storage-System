package com.unipi.application.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import com.unipi.application.config.Connection;

import reactor.core.publisher.Mono;

@Service
public class DeleteFileService {
    private static final Logger LOGGER = LoggerFactory.getLogger(DeleteFileService.class);

    public boolean deleteFile(String fileID, String jwtToken) {
        try {
            String deleteResponse = WebClient.create(Connection.BACKEND_URL)
                .delete()
                .uri(uriBuilder -> uriBuilder
                    .path("/delete")
                    .queryParam("fileID", fileID)
                    .build())
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                .exchangeToMono(response -> {
                    if (response.statusCode().isError()) {
                        LOGGER.error("Error on deleting file: {}", response.statusCode());
                        return Mono.just("error");
                    } else {
                        return response.bodyToMono(String.class);
                    }
                })
                .block();

            if (deleteResponse != null && deleteResponse.equals("error")) {
                LOGGER.error("Delete failed for file: {}", fileID);
                return false;
            }
            LOGGER.info("Delete file complete for: {}. Response: {}", fileID, deleteResponse);
            // if the response status code is 204 No Content, delete was successful
            return true;

        } catch (Exception e) {
            LOGGER.error("Error deleting file: {}", e.getMessage());
            return false;
        }
    }
}