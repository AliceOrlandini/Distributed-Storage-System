package com.unipi.application.middleware;

import com.unipi.application.views.login.LoginView;
import com.unipi.application.views.registration.RegistrationView;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.server.ServiceInitEvent;
import com.vaadin.flow.server.VaadinServiceInitListener;
import com.vaadin.flow.server.VaadinSession;
import org.springframework.stereotype.Component;

@Component
public class Auth implements VaadinServiceInitListener {
    @Override
    public void serviceInit(ServiceInitEvent event) {
        event.getSource().addUIInitListener(uiInitEvent -> {
            final UI ui = uiInitEvent.getUI();
            ui.addBeforeEnterListener(beforeEnterEvent -> {
                if (beforeEnterEvent.getLocation().getPath().isEmpty()) {
                    if (VaadinSession.getCurrent().getAttribute("jwt") != null) {
                        beforeEnterEvent.forwardTo("upload");
                    } else {
                        beforeEnterEvent.forwardTo("login");
                    }
                } else if (requiresAuthentication(beforeEnterEvent.getNavigationTarget())
                        && VaadinSession.getCurrent().getAttribute("jwt") == null) {
                    beforeEnterEvent.forwardTo("login");
                }
            });
        });
    }


    private boolean requiresAuthentication(Class<?> navigationTarget) {
        // we exclude the public parts of the application, like the login and the register pages
        return !(navigationTarget.equals(LoginView.class) || navigationTarget.equals(RegistrationView.class));
    }
}
