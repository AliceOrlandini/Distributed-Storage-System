package com.unipi.application;
import java.util.concurrent.CountDownLatch;

import javafx.scene.Scene;
import javafx.scene.web.WebView;
import javafx.stage.Stage;

public class Application extends javafx.application.Application {

    // Latch per sincronizzare l'avvio del server con l'applicazione desktop
    private static CountDownLatch latch = new CountDownLatch(1);

    public static void releaseLatch() {
        latch.countDown();
    }
    @Override
    public void init() throws Exception {
        // Avvia Spring Boot in un thread separato
        new Thread(() -> {
            // Qui avvii l'applicazione Spring Boot
            VaadinApplication.launch(new String[]{});
            // Quando Spring Boot è pronto, decrementa il latch
            latch.countDown();
        }).start();
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        // Attendi fino a quando il latch non è rilasciato (cioè il server è pronto)
        latch.await();

        WebView webView = new WebView();

        // Imposta lo user agent se necessario
        webView.getEngine().setUserAgent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36");

        // Listener per il monitoraggio del caricamento della pagina
        webView.getEngine().getLoadWorker().stateProperty().addListener((obs, oldState, newState) -> {
            System.out.println("Stato caricamento: " + newState);
        });
        webView.getEngine().getLoadWorker().exceptionProperty().addListener((obs, oldExc, newExc) -> {
            if (newExc != null) {
                System.err.println("Errore nel caricamento della pagina: " + newExc.getMessage());
                newExc.printStackTrace();
            }
        });

        // Carica l'URL dell'applicazione Vaadin
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

