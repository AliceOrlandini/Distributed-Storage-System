package com.unipi.application.views.delete;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.vaadin.lineawesome.LineAwesomeIconUrl;

import com.unipi.application.model.FileDetails;
import com.unipi.application.services.DeleteFileService;
import com.unipi.application.services.GetFilesService;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.VaadinSession;

@PageTitle("Delete File")
@Route("delete")
@Menu(order = 5, icon = LineAwesomeIconUrl.TRASH_SOLID)
public class DeleteFileView extends VerticalLayout {
    private static final Logger LOGGER = LoggerFactory.getLogger(DeleteFileView.class);
    private final GetFilesService getFilesService = new GetFilesService();
    private final DeleteFileService deleteFileService = new DeleteFileService();

    public DeleteFileView() {
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

        Button deleteButton = new Button("Delete File");
        deleteButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        deleteButton.addClickListener(event -> {
            FileDetails selectedFile = fileComboBox.getValue();
            LOGGER.info("Deleting file {}", selectedFile);
            if (selectedFile != null) {
                boolean deleteSuccessful = deleteFileService.deleteFile(selectedFile.getFileID(), jwtToken);
                if (!deleteSuccessful) {
                    Notification.show("Error deleting file", 3000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_ERROR);
                    return;
                }
                Notification.show("File deleted successfully", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_SUCCESS);
                fileComboBox.setItems(getFilesService.getFiles(jwtToken));
            } else {
                Notification.show("Please select a file to delete", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
            }
        });

        add(fileComboBox, deleteButton);
        setAlignItems(FlexComponent.Alignment.CENTER);
        setJustifyContentMode(FlexComponent.JustifyContentMode.CENTER);
    }
}
