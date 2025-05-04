package com.unipi.application.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import com.unipi.application.config.Connection;

@Service
public class LoginService {
    private static final Logger LOGGER = LoggerFactory.getLogger(LoginService.class);

    public String authenticate(String username, String password) {
        try {
            LOGGER.info("Authentication trial for: {}", username);
            LoginRequest request = new LoginRequest(username, password);

            LoginResponse loginResponse = WebClient.create(Connection.BACKEND_URL)
                .post()
                .uri("/login")
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(request)
                .retrieve()
                .bodyToMono(LoginResponse.class)
                .block();
            if (loginResponse == null || loginResponse.getToken() == null) {
                LOGGER.error("Authentication failed for user: {} (no token returned)", username);
                return null;
            }
            LOGGER.info("Authentication complete: {}. Token received.", username);
            return loginResponse.getToken();
        } catch (WebClientResponseException e) {
            if (e.getStatusCode().value() == 400 || e.getStatusCode().value() == 401) {
                LOGGER.warn("Authentication failed for user: {} (invalid credentials)", username);
            } else {
                LOGGER.error("HTTP error during authentication for user: {}: {}", username, e.getMessage());
            }
            return null;
        } catch (Exception e) {
            LOGGER.error("Unexpected error during authentication for user: {}", username, e);
            return null;
        }
    }

    public static class LoginRequest {
        private String username;
        private String password;

        public LoginRequest() {}

        public LoginRequest(String username, String password) {
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

    public static class LoginResponse {
        private String token;
        
        public LoginResponse() {}

        public String getToken() {
            return token;
        }
    
        public void setToken(String token) {
            this.token = token;
        }
    }
}
