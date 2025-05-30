package com.unipi.application.views;

import java.util.List;

import com.unipi.application.views.login.LoginView;
import com.unipi.application.views.registration.RegistrationView;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.applayout.AppLayout;
import com.vaadin.flow.component.applayout.DrawerToggle;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.Footer;
import com.vaadin.flow.component.html.H1;
import com.vaadin.flow.component.html.Header;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.SvgIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.Scroller;
import com.vaadin.flow.component.sidenav.SideNav;
import com.vaadin.flow.component.sidenav.SideNavItem;
import com.vaadin.flow.router.Layout;
import com.vaadin.flow.server.VaadinSession;
import com.vaadin.flow.server.auth.AnonymousAllowed;
import com.vaadin.flow.server.menu.MenuConfiguration;
import com.vaadin.flow.server.menu.MenuEntry;
import com.vaadin.flow.theme.lumo.LumoUtility;

/**
 * The main view is a top-level placeholder for other views.
 */
@Layout
@AnonymousAllowed
public class MainLayout extends AppLayout {

    private H1 viewTitle;
    private Button logoutButton;

    public MainLayout() {
        setPrimarySection(Section.DRAWER);
        addDrawerContent();
        addHeaderContent();
    }

    private void addHeaderContent() {
        DrawerToggle toggle = new DrawerToggle();
        toggle.setAriaLabel("Menu toggle");

        viewTitle = new H1();
        viewTitle.addClassNames(LumoUtility.FontSize.LARGE, LumoUtility.Margin.NONE);

        logoutButton = new Button("Logout", event -> {
            VaadinSession.getCurrent().setAttribute("jwt", null);
            Notification.show("Logout done!", 3000, Notification.Position.TOP_CENTER);
            UI.getCurrent().navigate("login");
        });
        logoutButton.getStyle().set("margin-left", "auto");
        logoutButton.getStyle().set("margin-right", "16px");
        logoutButton.getStyle().set("margin-top", "4px");
        logoutButton.getStyle().set("margin-bottom", "4px");

        addToNavbar(true, toggle, viewTitle, logoutButton);
    }

    private void addDrawerContent() {
        Span appName = new Span("Distributed-Storage-System");
        appName.addClassNames(LumoUtility.FontWeight.SEMIBOLD, LumoUtility.FontSize.LARGE);
        Header header = new Header(appName);

        Scroller scroller = new Scroller(createNavigation());

        addToDrawer(header, scroller, createFooter());
    }

    private SideNav createNavigation() {
        SideNav nav = new SideNav();

        List<MenuEntry> menuEntries = MenuConfiguration.getMenuEntries();
        menuEntries.forEach(entry -> {
            if (entry.icon() != null) {
                nav.addItem(new SideNavItem(entry.title(), entry.path(), new SvgIcon(entry.icon())));
            } else {
                nav.addItem(new SideNavItem(entry.title(), entry.path()));
            }
        });

        return nav;
    }

    private Footer createFooter() {
        Footer layout = new Footer();

        return layout;
    }

    @Override
    protected void afterNavigation() {
        super.afterNavigation();
        viewTitle.setText(getCurrentPageTitle());
        Class<?> currentView = UI.getCurrent().getInternals().getActiveRouterTargetsChain().isEmpty()
            ? null
            : UI.getCurrent().getInternals().getActiveRouterTargetsChain().get(0).getClass();
        if (logoutButton != null) {
            boolean showLogout = currentView != null &&
                !LoginView.class.isAssignableFrom(currentView) &&
                !RegistrationView.class.isAssignableFrom(currentView);
            logoutButton.setVisible(showLogout);
        }
    }

    private String getCurrentPageTitle() {
        return MenuConfiguration.getPageHeader(getContent()).orElse("");
    }
}
