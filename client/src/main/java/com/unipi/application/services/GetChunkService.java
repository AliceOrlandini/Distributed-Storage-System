package com.unipi.application.services;

import com.unipi.application.model.ChunckModel;
import com.unipi.application.model.FilePositionModel;
import com.vaadin.flow.server.VaadinSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

@Service
public class GetChunkService {

    private static final Logger LOGGER = LoggerFactory.getLogger(GetChunkService.class);

    public ChunckModel getChunk(FilePositionModel filePositionModel, String jwtToken) {
        try {
            String backendUrl = "http://localhost:5000";
            LOGGER.info("Fetching chunk for file position {} from {}", filePositionModel, backendUrl);

            ChunckModel chunk = WebClient.create(backendUrl)
                    .get()
                    .uri(uriBuilder -> uriBuilder
                            .path("/chunk")
                            .queryParam("ip", filePositionModel.getIp())
                            .queryParam("chunkHash", filePositionModel.getChunkHash())
                            .queryParam("chunkPosition", filePositionModel.getChunkPosition())
                            .build())
                    .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                    .exchangeToMono(response -> {
                        if (response.statusCode().isError()) {
                            LOGGER.error("Error fetching chunk, status code: {}", response.statusCode());
                            return Mono.empty();
                        } else {
                            return response.bodyToMono(ChunckModel.class);
                        }
                    })
                    .block();

            if (chunk == null || chunk.getData() == null || chunk.getData().length == 0) {
                LOGGER.error("Error fetching chunk: received null or empty data");
                return null;
            }

            // set the position of the chunk
            chunk.setPositionFromFilePosition(filePositionModel);
            return chunk;
        } catch (Exception e) {
            LOGGER.error("Error fetching chunk", e);
            throw new RuntimeException("Error fetching chunk", e);
        }
    }
}
