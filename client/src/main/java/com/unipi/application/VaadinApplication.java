package com.unipi.application;

import com.vaadin.flow.component.page.AppShellConfigurator;
import com.vaadin.flow.theme.Theme;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
/**
 * The entry point of the Spring Boot application.
 * Use the @PWA annotation make the application installable on phones, tablets
 * and some desktop browsers.
 *
 */
@SpringBootApplication
@Theme(value = "distributed-storage-system")
public class VaadinApplication implements AppShellConfigurator {

    public static void launch(String[] args) {
        SpringApplication app = new SpringApplication(VaadinApplication.class);
        // Aggiungi un listener per l'evento di applicazione pronta
        app.addListeners((ApplicationListener<ApplicationReadyEvent>) event -> {
            Application.releaseLatch();
        });
        app.run(args);    }

}
