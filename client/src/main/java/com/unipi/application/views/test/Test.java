package com.unipi.application.views.test;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.router.Menu;
import com.vaadin.flow.router.PageTitle;
import com.vaadin.flow.router.Route;
import org.vaadin.lineawesome.LineAwesomeIconUrl;

import java.nio.file.*;
import java.io.IOException;

@PageTitle("Test")
@Route("")
@Menu(order = 0, icon = LineAwesomeIconUrl.GLOBE_SOLID)
public class Test extends HorizontalLayout {

    private final Div fileContentDiv = new Div();

    public Test() {

        Button refreshButton = new Button("Refresh", event -> {
            loadFileContent();
        });
        refreshButton.setClassName("refresh-button");
        add(refreshButton, fileContentDiv);
        loadFileContent();

    }

    private void loadFileContent() {
        try {
            String filePath = "./src/main/resources/test.txt";
            String content = Files.readString(Paths.get(filePath));
            fileContentDiv.setText(content);
        } catch(IOException e) {
            fileContentDiv.setText("Error loading file content: " + e.getMessage());
        }
    }

}
