package com.unipi.application.services;

import com.vaadin.flow.server.VaadinSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

@Service
public class RegistrationService {
    private static final Logger LOGGER = LoggerFactory.getLogger(RegistrationService.class);

    public boolean register(String username, String password) {
        try {
            LOGGER.info("Registration trial for: {}", username);
            RegistrationRequest request = new RegistrationRequest(username, password);

            String backendUrl = "http://localhost:5000";
            String registrationResponse = WebClient.create(backendUrl)
                    .post()
                    .uri("/register")
                    .contentType(MediaType.APPLICATION_JSON)
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

            if (registrationResponse != null && registrationResponse.equals("error")) {
                LOGGER.error("Registration failed for user: {}", username);
                return false;
            }
            LOGGER.info("Registration complete for: {}. Response: {}", username, registrationResponse);
            // if the response status code is 204 No Content, the registration was successful
            return true;
        } catch (Exception e) {
            LOGGER.error("Registration failed for user: {}", username, e);
            return false;
        }
    }

    public static class RegistrationRequest {
        private String username;
        private String password;

        public RegistrationRequest() {}

        public RegistrationRequest(String username, String password) {
            this.username = username;
            this.password = password;
        }

        public String getUsername() {
            return username;
        }
        public void setUsername(String username) {
            this.username = username;
        }
        public String getPassword() {
            return password;
        }
        public void setPassword(String password) {
            this.password = password;
        }
    }
}
