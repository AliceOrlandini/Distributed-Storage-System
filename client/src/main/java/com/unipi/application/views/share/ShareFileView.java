package com.unipi.application.views.share;

import com.unipi.application.model.FileDetails;
import com.unipi.application.services.ShareService;
import com.unipi.application.services.GetFilesService;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.VaadinSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.vaadin.lineawesome.LineAwesomeIconUrl;

import java.io.IOException;
import java.util.List;


@PageTitle("Share File")
@Route("share")
@Menu(order = 4, icon = LineAwesomeIconUrl.SHARE_SOLID)
public class ShareFileView extends VerticalLayout {

    private static final Logger LOGGER = LoggerFactory.getLogger(ShareFileView.class);
    private final ShareService shareService = new ShareService();

    public ShareFileView() {
        String jwtToken = VaadinSession.getCurrent().getAttribute("jwt").toString();
        ComboBox<FileDetails> fileComboBox = new ComboBox<>("Select a file");
        GetFilesService getFilesService = new GetFilesService();
        List<FileDetails> files = getFilesService.getFiles(jwtToken);
        if (files.isEmpty()) {
            Notification.show("No files available", 10000, Notification.Position.TOP_CENTER)
                    .addThemeVariants(NotificationVariant.LUMO_WARNING);
        }
        fileComboBox.setItems(files);

        fileComboBox.setItemLabelGenerator(FileDetails::getFileName);
        fileComboBox.setPlaceholder("Choose a file");

        // add an input box for the username to share the file with
        TextField userTextField = new TextField();
        userTextField.setLabel("Username");
        userTextField.setPlaceholder("kevin");
        userTextField.setClearButtonVisible(true);
        userTextField.setPrefixComponent(VaadinIcon.USER.create());

        Button downloadButton = new Button("Share File");
        downloadButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        downloadButton.addClickListener(event -> {
            FileDetails selectedFile = fileComboBox.getValue();
            if (userTextField.getValue() == null || userTextField.getValue().isEmpty()) {
                Notification.show("Enter a username", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
                return;
            }
            try {
                boolean shareSuccessful = shareService.shareFile(userTextField.getValue(), selectedFile.getFileID(), jwtToken);
                if (!shareSuccessful) {
                    Notification.show("Error sharing file", 3000, Notification.Position.TOP_CENTER)
                            .addThemeVariants(NotificationVariant.LUMO_ERROR);
                    return;
                }
                Notification.show("File shared successfully", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_SUCCESS);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            LOGGER.info("Sharing file {} to {}", selectedFile, userTextField.getValue());
        });

        add(fileComboBox, userTextField, downloadButton);
        setAlignItems(Alignment.CENTER);
        setJustifyContentMode(JustifyContentMode.CENTER);
    }
}
