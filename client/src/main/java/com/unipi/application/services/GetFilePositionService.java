package com.unipi.application.services;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import com.unipi.application.model.FilePositionModel;

import reactor.core.publisher.Flux;

@Service
public class GetFilePositionService {

    private static final Logger LOGGER = LoggerFactory.getLogger(GetFilePositionService.class);

    public List<FilePositionModel> getFilePositions(String jwtToken, String fileName) {
        try {
            String backendUrl = "http://localhost:8080";
            LOGGER.info("Fetching file positions from {}", backendUrl);

            List<FilePositionModel> filePositions = WebClient.create(backendUrl)
                    .get()
                    .uri(uriBuilder -> uriBuilder
                            .path("/fileparts")
                            .queryParam("file", fileName)
                            .build())
                    .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                    .exchangeToFlux(response -> {
                        if (response.statusCode().isError()) {
                            LOGGER.error("Error fetching file positions, status code: {}", response.statusCode());
                            return Flux.empty();
                        } else {
                            return response.bodyToFlux(FilePositionModel.class);
                        }
                    })
                    .collectList()
                    .block();

            if (filePositions == null || filePositions.isEmpty()) {
                LOGGER.error("No file positions were fetched");
                return List.of();
            }
            return filePositions;
        } catch (Exception e) {
            LOGGER.error("Exception fetching file positions", e);
            throw new RuntimeException("Error fetching file positions", e);
        }
    }
}
