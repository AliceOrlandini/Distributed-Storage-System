package com.unipi.application.views.login;

import com.unipi.application.services.LoginService;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.login.LoginForm;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.notification.NotificationVariant;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.BeforeEnterObserver;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.VaadinSession;


@Route(value = "login")
public class LoginView extends VerticalLayout implements BeforeEnterObserver {

    private final LoginService loginService = new LoginService();

    public LoginView() {
        LoginForm loginForm = new LoginForm();
        loginForm.setForgotPasswordButtonVisible(false);
        loginForm.setClassName("centered-content");
        Button registrationButton = new Button("Not registered yet? Sign up", new Icon(VaadinIcon.ARROW_RIGHT));
        registrationButton.addClickListener(e -> UI.getCurrent().navigate("registration"));
        registrationButton.addThemeVariants(ButtonVariant.LUMO_TERTIARY);
        registrationButton.setIconAfterText(true);
        registrationButton.setClassName("centered-content padding-x");
        add(loginForm);
        add(registrationButton);

        loginForm.addLoginListener(event -> {
            String username = event.getUsername();
            String password = event.getPassword();
            String token = loginService.authenticate(username, password);
            if (token != null) {
                VaadinSession.getCurrent().setAttribute("jwt", token);
                Notification notification = new Notification("Login successful");
                notification.addThemeVariants(NotificationVariant.LUMO_SUCCESS);
                notification.setPosition(Notification.Position.TOP_CENTER);
                notification.setDuration(10000);
                notification.open();
                UI.getCurrent().navigate("upload");
            } else {
                loginForm.showErrorMessage("Invalid login", "Username or password is incorrect");
                loginForm.setError(true);
            }
        });
    }

    @Override
    public void beforeEnter(BeforeEnterEvent event) {
        if (VaadinSession.getCurrent().getAttribute("jwt") != null) {
            event.forwardTo("upload");
        }
    }
}
