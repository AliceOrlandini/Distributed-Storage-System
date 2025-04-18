package com.unipi.application.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;
import java.io.IOException;
import com.unipi.application.config.Connection;

@Service
public class ShareService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ShareService.class);

    public boolean shareFile(String username, String fileName, String jwtToken) throws IOException {
        LOGGER.info("Sharing file: {} to {}", fileName, Connection.BACKEND_URL);
        ShareFileBody request = new ShareFileBody(username, fileName);
        try {
            String shareResponse = WebClient.create(Connection.BACKEND_URL)
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

            if (shareResponse != null && shareResponse.equals("error")) {
                LOGGER.error("Share failed for file: {}", fileName);
                return false;
            }
            LOGGER.info("Share file complete for: {}. Response: {}", fileName, shareResponse);
            // if the response status code is 204 No Content, share was successful
            return true;

        } catch (Exception e) {
            LOGGER.error("Exception occurred while uploading file: {}", e.getMessage());
            return false;
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
