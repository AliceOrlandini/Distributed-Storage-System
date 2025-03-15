package com.unipi.application.services;

import com.unipi.application.model.Chunck;
import com.unipi.application.model.FilePosition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

@Service
public class GetChunkService {

    private String backendUrl = "http://localhost:5000";

    private static final Logger LOGGER = LoggerFactory.getLogger(GetChunkService.class);

    public Chunck getChunck(FilePosition filePosition) {
        try {
            LOGGER.info("Fetching chunk for file position {} from {}", filePosition, backendUrl);
            Chunck chunck = WebClient.create(backendUrl)
                    .get()
                    .uri(uriBuilder -> uriBuilder
                            .path("/chunk")
                            .queryParam("ip", filePosition.getIp())
                            .queryParam("chunkHash", filePosition.getChunkHash())
                            .queryParam("chunkPosition", filePosition.getChunkPosition())
                            .build())
                    //.header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                    .retrieve()
                    .bodyToMono(Chunck.class)
                    .block();

            if (chunck != null) {
                chunck.setPositionFromFilePosition(filePosition);
            }

            return chunck;
        } catch (Exception e) {
            LOGGER.error("Error fetching chunk", e);
            throw new RuntimeException("Error fetching chunk", e);
        }
    }
}
