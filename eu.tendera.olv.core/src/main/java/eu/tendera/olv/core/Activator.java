package eu.tendera.olv.core;

import java.util.Dictionary;
import java.util.Hashtable;

import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.http.jetty.JettyConfigurator;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import eu.tendera.olv.core.preferences.PreferenceConstants;

public class Activator extends AbstractUIPlugin implements IStartup {

  public static final String PLUGIN_ID = "OtrosLogViewerPlugin"; //$NON-NLS-1$

  private static Activator plugin;

  public Activator() {
  }

  @Override
  public void start(BundleContext context) throws Exception {
    super.start(context);
    plugin = this;

    final Bundle bundle = Platform.getBundle("org.eclipse.equinox.http.registry");
    if (bundle.getState() == Bundle.RESOLVED) {
      bundle.start(Bundle.START_TRANSIENT);
    }

    if (getPreferenceStore().getBoolean(PreferenceConstants.ENABLED)) {
      startServlet(getPreferenceStore().getInt(PreferenceConstants.PORT));
    }
  }

  @Override
  public void stop(BundleContext context) throws Exception {
    plugin = null;
    super.stop(context);
  }

  @Override
  public void earlyStartup() {
  }

  public static Activator getDefault() {
    return plugin;
  }
  
  private void startServlet(int portNumber) {
    final Dictionary<String, Comparable<?>> settings = new Hashtable<>();
    settings.put("http.enabled", Boolean.TRUE);
    settings.put("http.port", portNumber);
    settings.put("http.host", "0.0.0.0");
    settings.put("https.enabled", Boolean.FALSE);
    settings.put("context.path", "/");
    settings.put("context.sessioninactiveinterval", 1800);

    try {
      JettyConfigurator.startServer(PLUGIN_ID + ".jetty", settings);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
