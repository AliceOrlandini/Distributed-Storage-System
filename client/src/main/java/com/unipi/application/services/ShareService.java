package com.unipi.application.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.io.IOException;

@Service
public class ShareService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ShareService.class);

    public boolean shareFile(String username, String fileName, String jwtToken) throws IOException {
        String backendUrl = "http://localhost:8080";
        LOGGER.info("Uploading file: {} to {}", fileName, backendUrl);
        ShareFileBody request = new ShareFileBody(username, fileName);
        try {
            String registrationResponse = WebClient.create(backendUrl)
                    .post()
                            .uri("/share")
                    .contentType(MediaType.APPLICATION_JSON)
                    .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtToken)
                    .bodyValue(request)
                    .exchangeToMono(response -> {
                        if (response.statusCode().isError()) {
                            LOGGER.error("Error on client: {}", response.statusCode());
                            return Mono.just("error");
                        } else {
                            return response.bodyToMono(String.class);
                        }
                    })
                    .block();

            return !"error".equals(registrationResponse);
        } catch (Exception e) {
            LOGGER.error("Exception occurred while uploading file: {}", e.getMessage());
            throw new IOException("Error uploading file", e);
        }
    }

    private static class ShareFileBody {
        public String fileID;
        public String username;

        public ShareFileBody() {}

        public ShareFileBody(String username, String fileID) {
            this.username = username;
            this.fileID = fileID;
        }
    }
}
