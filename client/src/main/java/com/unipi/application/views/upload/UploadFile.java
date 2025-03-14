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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.vaadin.lineawesome.LineAwesomeIconUrl;
import java.io.InputStream;

@PageTitle("Upload File")
@Route("upload")
@Menu(order = 1, icon = LineAwesomeIconUrl.GLOBE_SOLID)
public class UploadFile extends Div {

    private static final Logger LOGGER = LoggerFactory.getLogger(UploadFile.class);
    private final UploadFileService uploadFileService = new UploadFileService();

    public UploadFile() {

        MultiFileMemoryBuffer buffer = new MultiFileMemoryBuffer();
        Upload upload = new Upload(buffer);

        upload.addSucceededListener(event -> {
            String fileName = event.getFileName();
            InputStream inputStream = buffer.getInputStream(fileName);
            Notification notification;

            try {
                boolean result = uploadFileService.uploadFile(inputStream, fileName);
                if (result) {
                    notification = Notification
                            .show("File uploaded successfully");
                    notification.addThemeVariants(NotificationVariant.LUMO_SUCCESS);
                    notification.setDuration(10000);
                } else {
                    notification = Notification
                            .show("Error during file upload");
                    notification.addThemeVariants(NotificationVariant.LUMO_ERROR);
                }
                notification.setPosition(Notification.Position.TOP_END);
                notification.setDuration(10000);
            } catch (Exception e) {
                LOGGER.error("Error uploading file", e);
                notification = Notification
                        .show("An exception occurred: " + e.getMessage());
                notification.addThemeVariants(NotificationVariant.LUMO_ERROR);
                notification.setPosition(Notification.Position.TOP_END);
                notification.setDuration(10000);
            }
        });

        add(upload);
    }

}
