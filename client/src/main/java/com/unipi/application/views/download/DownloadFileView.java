package com.unipi.application.views.download;

import com.unipi.application.model.Chunck;
import com.unipi.application.model.FilePosition;
import com.unipi.application.services.GetChunkService;
import com.unipi.application.services.GetFilePositionService;
import com.unipi.application.services.GetFilesService;
import com.unipi.application.services.SaveFileService;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.vaadin.lineawesome.LineAwesomeIconUrl;
import java.util.stream.Collectors;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@PageTitle("Download File")
@Route("download")
@Menu(order = 3, icon = LineAwesomeIconUrl.DOWNLOAD_SOLID)
public class DownloadFileView extends VerticalLayout {

    private static final Logger LOGGER = LoggerFactory.getLogger(DownloadFileView.class);

    private final SaveFileService saveFile = new SaveFileService();
    private final GetFilePositionService getFilePositionService = new GetFilePositionService();
    private final GetChunkService getChunkService = new GetChunkService();
    private final GetFilesService getFilesService = new GetFilesService();

    public DownloadFileView() {
        ComboBox<String> fileComboBox = new ComboBox<>("Seleziona un file");
        List<String> files = getFilesService.getFiles();
        fileComboBox.setItems(files);
        fileComboBox.setPlaceholder("Scegli un file");


        Button downloadButton = new Button("Scarica e Salva File");
        downloadButton.addClickListener(event -> {
            String selectedFile = fileComboBox.getValue();
            String savePath = System.getProperty("user.home") + "/Downloads/" + selectedFile;

            if (selectedFile == null || selectedFile.isEmpty()) {
                Notification.show("Seleziona un file", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
                return;
            }
            LOGGER.info("Downloading file " + selectedFile + " to " + savePath);

            // takes the ip addresses of the servers that have the chunks of the selected files
            List<FilePosition> filePositions = getFilePositionService.getFilePositions();
            List<Chunck> chunks = filePositions.parallelStream()
                    .map(getChunkService::getChunck)
                    .toList();


            try {
                saveFile.SaveFile(chunks, savePath);
                Notification.show("File scaricato e salvato in " + savePath,
                                5000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_SUCCESS);
            } catch (IOException e) {
                LOGGER.error("Errore nel download del file", e);
                Notification.show("Errore durante il salvataggio: " + e.getMessage(),
                                5000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
            }
        });

        add(fileComboBox, downloadButton);
        setAlignItems(Alignment.CENTER);
        setJustifyContentMode(JustifyContentMode.CENTER);
    }
}
