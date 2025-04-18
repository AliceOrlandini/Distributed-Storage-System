package com.unipi.application.views.download;

import com.unipi.application.model.FileDetails;
import com.unipi.application.model.ChunckModel;
import com.unipi.application.model.FilePositionModel;
import com.unipi.application.services.GetChunkService;
import com.unipi.application.services.GetFilePositionService;
import com.unipi.application.services.GetFilesService;
import com.unipi.application.services.SaveFileService;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.VaadinSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.vaadin.lineawesome.LineAwesomeIconUrl;

import java.io.IOException;
import java.util.List;
import java.util.Objects;

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
        String jwtToken = VaadinSession.getCurrent().getAttribute("jwt").toString();
        ComboBox<FileDetails> fileComboBox = new ComboBox<>("Select a file");
        List<FileDetails> files = getFilesService.getFiles(jwtToken);
        if (files.isEmpty()) {
            Notification.show("No files available", 10000, Notification.Position.TOP_CENTER)
                    .addThemeVariants(NotificationVariant.LUMO_WARNING);
        }

        fileComboBox.setItems(files);

        fileComboBox.setItemLabelGenerator(FileDetails::getFileName);
        fileComboBox.setPlaceholder("Choose a file");


        Button downloadButton = new Button("Download and Save");
        downloadButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        downloadButton.addClickListener(event -> {
            FileDetails selectedFile = fileComboBox.getValue();

            String savePath = System.getProperty("user.home") + "/Downloads/" + selectedFile.getFileName();

            LOGGER.info("Downloading file {} to {}", selectedFile, savePath);

            // takes the ip addresses of the servers that have the chunks of the selected files
            List<FilePositionModel> filePositions = getFilePositionService.getFilePositions(jwtToken, selectedFile.getFileID());
            if (filePositions.isEmpty()) {
                Notification.show("No file positions available", 10000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
                return;
            }
            List<ChunckModel> chunks = filePositions.parallelStream()
                    .map(filePosition -> getChunkService.getChunk(filePosition, jwtToken))
                    .filter(Objects::nonNull)
                    .toList();

            if (chunks.isEmpty()) {
                Notification.show("No chunks available", 10000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
                return;
            }

            try {
                boolean savedSuccessfully = saveFile.saveFile(chunks, savePath);
                if (!savedSuccessfully) {
                    Notification.show("File not saved", 5000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_ERROR);
                    return;
                }
                Notification.show("File downloaded and saved in " + savePath,
                                5000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_SUCCESS);
            } catch (IOException e) {
                LOGGER.error("Error during download", e);
                Notification.show("Error during saving: " + e.getMessage(),
                                5000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
            }
        });

        add(fileComboBox, downloadButton);
        setAlignItems(Alignment.CENTER);
        setJustifyContentMode(JustifyContentMode.CENTER);
    }
}
