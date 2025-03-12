package com.unipi.application.views.upload;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.component.upload.receivers.MultiFileMemoryBuffer;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import org.vaadin.lineawesome.LineAwesomeIconUrl;

import java.io.InputStream;

@PageTitle("Upload")
@Route("upload")
@Menu(order = 1, icon = LineAwesomeIconUrl.GLOBE_SOLID)
public class UploadFile extends Div {

    public UploadFile() {
        // tag::snippet[]
        MultiFileMemoryBuffer buffer = new MultiFileMemoryBuffer();
        Upload upload = new Upload(buffer);
        // upload.setDropAllowed(false);

        upload.addSucceededListener(event -> {
            String fileName = event.getFileName();
            InputStream inputStream = buffer.getInputStream(fileName);

            // Do something with the file data
            // processFile(inputStream, fileName);
        });
        // end::snippet[]


        add(upload);
    }

}
