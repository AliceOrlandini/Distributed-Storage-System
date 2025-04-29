package com.unipi.application;
import java.util.concurrent.CountDownLatch;

import javafx.scene.Scene;
import javafx.scene.web.WebView;
import javafx.stage.Stage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Application extends javafx.application.Application {

    // latch to synchronize the Springboot application and the JavaFX
    private static final CountDownLatch latch = new CountDownLatch(1);
    private static final Logger LOGGER = LoggerFactory.getLogger(Application.class);

    public static void releaseLatch() {
        latch.countDown();
    }
    @Override
    public void init() throws Exception {
        // start the Springboot application in a separated thread
        new Thread(() -> {
            VaadinApplication.main(new String[]{});
            // when the Springboot application is ready decrement the latch
            latch.countDown();
        }).start();
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        // wait until the spring application is ready
        latch.await();

        WebView webView = new WebView();
        webView.getEngine().setUserAgent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36");

        webView.getEngine().getLoadWorker().stateProperty().addListener((obs, oldState, newState) -> {
            System.out.println("Loading Status: " + newState);
        });
        webView.getEngine().getLoadWorker().exceptionProperty().addListener((obs, oldExc, newExc) -> {
            if (newExc != null) {
                LOGGER.error("Error on loading the page: {}", newExc.getMessage());
            }
        });

        webView.getEngine().load("http://localhost:3000/");

        Scene scene = new Scene(webView, 1024, 768);
        primaryStage.setTitle("Distributed storage system");
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}

