package com.unipi.application.views.management;

import java.io.IOException;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.vaadin.lineawesome.LineAwesomeIconUrl;

import com.unipi.application.model.ChunckModel;
import com.unipi.application.model.FileDetails;
import com.unipi.application.model.FilePositionModel;
import com.unipi.application.services.DeleteFileService;
import com.unipi.application.services.GetChunkService;
import com.unipi.application.services.GetFilePositionService;
import com.unipi.application.services.GetFilesService;
import com.unipi.application.services.SaveFileService;
import com.unipi.application.services.ShareService;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.VaadinSession;

@PageTitle("File Management")
@Route("file-management")
@Menu(order = 3, icon = LineAwesomeIconUrl.DOWNLOAD_SOLID)
public class FileManagementView extends VerticalLayout {
    private static final Logger LOGGER = LoggerFactory.getLogger(FileManagementView.class);

    private final SaveFileService saveFile = new SaveFileService();
    private final GetFilePositionService getFilePositionService = new GetFilePositionService();
    private final GetChunkService getChunkService = new GetChunkService();
    private final GetFilesService getFilesService = new GetFilesService();
    private final DeleteFileService deleteFileService = new DeleteFileService();
    private final ShareService shareService = new ShareService();

    public FileManagementView() {
        String jwtToken = VaadinSession.getCurrent().getAttribute("jwt").toString();
        List<FileDetails> files = getFilesService.getFiles(jwtToken);
        if (files.isEmpty()) {
            Notification.show("No files available", 10000, Notification.Position.TOP_CENTER)
                    .addThemeVariants(NotificationVariant.LUMO_WARNING);
        }

        Grid<FileDetails> fileGrid = new Grid<>(FileDetails.class, false);
        fileGrid.addColumn(FileDetails::getFileName).setHeader("File Name");
        fileGrid.addComponentColumn(fileDetails -> {
            Button downloadButton = new Button("Download");
            downloadButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
            downloadButton.addClassName("custom-cursor");
            downloadButton.addClickListener(event -> {
                String savePath = System.getProperty("user.home") + "/Downloads/" + fileDetails.getFileName();
                LOGGER.info("Downloading file {} to {}", fileDetails, savePath);
                List<FilePositionModel> filePositions = getFilePositionService.getFilePositions(jwtToken, fileDetails.getFileID());
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
            return downloadButton;
        }).setHeader("Download");
        fileGrid.addComponentColumn(fileDetails -> {
            Button deleteButton = new Button("Delete");
            deleteButton.addThemeVariants(ButtonVariant.LUMO_ERROR);
            deleteButton.addClassName("custom-cursor");
            deleteButton.getStyle().set("color", "#fff");
            deleteButton.getStyle().set("background-color", "#d32f2f");
            deleteButton.addClickListener(event -> {
                boolean deleteSuccessful = deleteFileService.deleteFile(fileDetails.getFileID(), jwtToken);
                if (!deleteSuccessful) {
                    Notification.show("Error deleting file", 3000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_ERROR);
                    return;
                }
                Notification.show("File deleted successfully", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_SUCCESS);
                // Update the grid after deletion
                fileGrid.setItems(getFilesService.getFiles(jwtToken));
            });
            return deleteButton;
        }).setHeader("Delete");
        fileGrid.addComponentColumn(fileDetails -> {
            Button shareButton = new Button("Share");
            shareButton.addThemeVariants(ButtonVariant.LUMO_SUCCESS);
            shareButton.addClassName("custom-cursor");
            shareButton.getStyle().set("color", "#fff");
            shareButton.getStyle().set("background-color", "#43a047"); // green
            shareButton.addClickListener(event -> {
                Dialog dialog = new Dialog();
                TextField userField = new TextField("Username");
                userField.setPrefixComponent(VaadinIcon.USER.create());
                userField.setClearButtonVisible(true);
                Button confirm = new Button("Share", e -> {
                    String username = userField.getValue();
                    if (username == null || username.isEmpty()) {
                        Notification.show("Enter a username", 3000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_ERROR);
                        return;
                    }
                    boolean shared;
                    try {
                        shared = shareService.shareFile(username, fileDetails.getFileID(), jwtToken);
                    } catch (Exception ex) {
                        Notification.show("Error sharing: " + ex.getMessage(), 3000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_ERROR);
                        return;
                    }
                    if (shared) {
                        Notification.show("File shared!", 3000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_SUCCESS);
                        dialog.close();
                    } else {
                        Notification.show("Error sharing file", 3000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_ERROR);
                    }
                });
                confirm.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
                dialog.add(userField, confirm);
                dialog.open();
            });
            return shareButton;
        }).setHeader("Share");
        fileGrid.setItems(files);
        fileGrid.setWidthFull();
        fileGrid.setAllRowsVisible(true);

        add(fileGrid);
        setAlignItems(Alignment.CENTER);
        setJustifyContentMode(JustifyContentMode.CENTER);
    }
}
