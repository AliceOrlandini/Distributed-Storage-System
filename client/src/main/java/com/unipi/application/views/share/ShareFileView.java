package com.unipi.application.views.share;

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

import java.util.List;

@PageTitle("Share File")
@Route("share")
@Menu(order = 4, icon = LineAwesomeIconUrl.SHARE_SOLID)
public class ShareFileView extends VerticalLayout {
    private static final Logger LOGGER = LoggerFactory.getLogger(ShareFileView.class);

    public ShareFileView() {
        String jwtToken = VaadinSession.getCurrent().getAttribute("jwt").toString();
        ComboBox<String> fileComboBox = new ComboBox<>("Select a file");
        GetFilesService getFilesService = new GetFilesService();
        List<String> files = getFilesService.getFiles(jwtToken);
        if (files.isEmpty()) {
            Notification.show("No files available", 10000, Notification.Position.TOP_CENTER)
                    .addThemeVariants(NotificationVariant.LUMO_WARNING);
        }
        fileComboBox.setItems(files);
        fileComboBox.setPlaceholder("Choose a file");

        // add an input box for the username to share the file with
        TextField userTextField = new TextField();
        userTextField.setLabel("Username");
        userTextField.setPlaceholder("Kevin");
        userTextField.setClearButtonVisible(true);
        userTextField.setPrefixComponent(VaadinIcon.USER.create());

        Button downloadButton = new Button("Share File");
        downloadButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        downloadButton.addClickListener(event -> {
            String selectedFile = fileComboBox.getValue();
            if (selectedFile == null || selectedFile.isEmpty()) {
                Notification.show("Select a file", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
                return;
            }
            if (userTextField.getValue() == null || userTextField.getValue().isEmpty()) {
                Notification.show("Enter a username", 3000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
                return;
            }
            LOGGER.info("Sharing file {} to {}", selectedFile, userTextField.getValue());
        });

        add(fileComboBox, userTextField, downloadButton);
        setAlignItems(Alignment.CENTER);
        setJustifyContentMode(JustifyContentMode.CENTER);
    }
}
