package com.unipi.application.views.registration;

import com.unipi.application.model.RegistrationModel;
import com.unipi.application.services.RegistrationService;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.PasswordField;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.router.Route;

@Route("registration")
public class RegistrationView extends Div {

    private final Binder<RegistrationModel> binder;
    private final RegistrationService registrationService = new RegistrationService();

    public RegistrationView() {
        H2 title = new H2("Registration");
        title.setClassName("w-registration centered-content");

        TextField usernameField = new TextField("Username");
        usernameField.setClassName("w-registration centered-content");

        PasswordField passwordField = new PasswordField("Password");
        passwordField.setClassName("w-registration centered-content");

        PasswordField confirmPasswordField = new PasswordField("Confirm Password");
        confirmPasswordField.setClassName("w-registration centered-content");

        Button registerButton = new Button("Sign Up");
        registerButton.setClassName("w-registration centered-content");
        registerButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);

        Button backToLoginButton = new Button("Back to Login", new Icon(VaadinIcon.ARROW_RIGHT));
        backToLoginButton.setClassName("w-registration centered-content");
        backToLoginButton.addThemeVariants(ButtonVariant.LUMO_TERTIARY);

        VerticalLayout content = new VerticalLayout();
        content.add(title, usernameField, passwordField, confirmPasswordField, registerButton, backToLoginButton);
        add(content);

        // the binder is used to bind the form fields to the RegistrationModel
        // in order to validate the input and to read the values
        binder = new Binder<>(RegistrationModel.class);

        binder.forField(usernameField)
                .asRequired("Username is required")
                .bind(RegistrationModel::getUsername, RegistrationModel::setUsername);

        binder.forField(passwordField)
                .asRequired("Password is required")
                .bind(RegistrationModel::getPassword, RegistrationModel::setPassword);

        binder.forField(confirmPasswordField)
                .asRequired("Confirm Password is required")
                // the field confirm password must be equal to the password field value
                .withValidator(confirmPassword -> confirmPassword.equals(passwordField.getValue()), "Le password non corrispondono")
                .bind(RegistrationModel::getConfirmPassword, RegistrationModel::setConfirmPassword);

        registerButton.addClickListener(event -> {
            RegistrationModel model = new RegistrationModel();

            if (binder.writeBeanIfValid(model)) {
                boolean registrationSuccessful = registrationService.register(model.getUsername(), model.getPassword());
                if (!registrationSuccessful) {
                    Notification notification = Notification.show("Error during registration, username already exists!");
                    notification.setPosition(Notification.Position.TOP_CENTER);
                    notification.addThemeVariants(NotificationVariant.LUMO_ERROR);
                    return;
                }
                Notification notification = Notification.show("Registration successful!");
                notification.setPosition(Notification.Position.TOP_CENTER);
                notification.addThemeVariants(NotificationVariant.LUMO_SUCCESS);
                notification.setDuration(10000);
                getUI().ifPresent(ui -> ui.navigate("login"));
            }
        });

        backToLoginButton.addClickListener(event -> {
            getUI().ifPresent(ui -> ui.navigate("login"));
        });
    }
}
