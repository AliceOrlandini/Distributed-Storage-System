package com.unipi.application.views.upload;

import com.unipi.application.services.UploadFileService;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.component.upload.receivers.MultiFileMemoryBuffer;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.server.VaadinSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.vaadin.lineawesome.LineAwesomeIconUrl;
import java.io.InputStream;

@PageTitle("Upload File")
@Route("upload")
@Menu(order = 1, icon = LineAwesomeIconUrl.CLOUD_UPLOAD_ALT_SOLID)
public class UploadFileView extends Div {

    private static final Logger LOGGER = LoggerFactory.getLogger(UploadFileView.class);
    private final UploadFileService uploadFileService = new UploadFileService();

    public UploadFileView() {
        MultiFileMemoryBuffer buffer = new MultiFileMemoryBuffer();
        Upload upload = new Upload(buffer);
        int maxFileSizeInBytes = 100 * 1024 * 1024; // 10MB
        upload.setMaxFileSize(maxFileSizeInBytes);
        upload.addFileRejectedListener(event -> {
            String errorMessage = event.getErrorMessage();

            Notification notification = Notification.show(errorMessage, 5000,
                    Notification.Position.MIDDLE);
            notification.addThemeVariants(NotificationVariant.LUMO_ERROR);
        });
        upload.addSucceededListener(event -> {
            String fileName = event.getFileName();
            InputStream inputStream = buffer.getInputStream(fileName);

            try {
                String jwtToken = VaadinSession.getCurrent().getAttribute("jwt").toString();
                boolean result = uploadFileService.uploadFile(inputStream, fileName, jwtToken);
                if (result) {
                    Notification.show("File uploaded successfully", 10000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_SUCCESS);
                } else {
                    Notification.show("Error during file upload", 10000, Notification.Position.TOP_CENTER)
                        .addThemeVariants(NotificationVariant.LUMO_ERROR);
                }
            } catch (Exception e) {
                LOGGER.error("Error uploading file", e);
                Notification.show("An exception occurred: " + e.getMessage(), 10000, Notification.Position.TOP_CENTER)
                    .addThemeVariants(NotificationVariant.LUMO_ERROR);
            }
        });

        add(upload);
    }

}
