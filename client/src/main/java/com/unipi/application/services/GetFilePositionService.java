package com.unipi.application.services;

import com.unipi.application.model.FilePosition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import java.util.List;

@Service
public class GetFilePositionService {

    private final String backendUrl = "http://localhost:5000";
    private static final Logger LOGGER = LoggerFactory.getLogger(GetFilePositionService.class);

    public List<FilePosition> getFilePositions() {
        try {
            LOGGER.info("Fetching file positions from {} ", backendUrl);
            return WebClient.create(backendUrl)
                    .get()
                    .uri("/filepositions")
                    .retrieve()
                    .bodyToFlux(FilePosition.class)
                    .collectList()
                    .block();

        } catch (Exception e) {
            LOGGER.error("Error fetching file positions", e);
            throw new RuntimeException("Error fetching file positions", e);
        }
    }
}
