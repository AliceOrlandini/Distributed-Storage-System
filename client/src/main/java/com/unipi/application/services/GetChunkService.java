package com.unipi.application.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import com.unipi.application.model.ChunckModel;
import com.unipi.application.model.FilePositionModel;
import reactor.core.publisher.Mono;

@Service
public class GetChunkService {

    private static final Logger LOGGER = LoggerFactory.getLogger(GetChunkService.class);

    public ChunckModel getChunk(FilePositionModel filePositionModel, String jwtToken) {
        try {
            String backendUrl = filePositionModel.getIp();
            LOGGER.info("Fetching chunk for file position {}", filePositionModel.getIp());

            byte[] data = WebClient.create(backendUrl)
                    .get()
                    .uri(uriBuilder -> uriBuilder
                            .path("/download")
                            .queryParam("token", filePositionModel.getChunkName())
                            .build())
                    .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                    .accept(MediaType.APPLICATION_OCTET_STREAM)
                    .exchangeToMono(response -> {
                        if (response.statusCode().isError()) {
                            LOGGER.error("Error fetching chunk, status code: {}", response.statusCode());
                            return Mono.empty();
                        } else {
                            return response.bodyToMono(byte[].class);
                        }
                    })
                    .block();

            ChunckModel chunk = new ChunckModel();
            chunk.setData(data);
            chunk.setPositionFromFilePosition(filePositionModel);
            return chunk;
        } catch (Exception e) {
            LOGGER.error("Error fetching chunk", e);
            throw new RuntimeException("Error fetching chunk", e);
        }
    }
}
