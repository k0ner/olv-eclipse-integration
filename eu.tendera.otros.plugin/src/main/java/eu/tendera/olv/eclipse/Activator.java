package eu.tendera.olv.eclipse;

import static eu.tendera.olv.eclipse.preferences.PreferenceConstants.DEFAULT_ENABLED;
import static eu.tendera.olv.eclipse.preferences.PreferenceConstants.DEFAULT_PORT;
import static eu.tendera.olv.eclipse.preferences.PreferenceConstants.ENABLED;
import static eu.tendera.olv.eclipse.preferences.PreferenceConstants.PORT;

import java.util.Dictionary;
import java.util.Hashtable;

import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.http.jetty.JettyConfigurator;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import eu.tendera.olv.eclipse.preferences.PreferenceConstants;

public class Activator extends AbstractUIPlugin implements IStartup {

  public static final String PLUGIN_ID = "OtrosLogViewerPlugin"; //$NON-NLS-1$

  private static Activator plugin;

  public Activator() {
  }

  @Override
  public void start(BundleContext context) throws Exception {
    initializeValues();
    super.start(context);
    plugin = this;

    final Bundle bundle = Platform.getBundle("org.eclipse.equinox.http.registry");
    if (bundle.getState() == Bundle.RESOLVED) {
      bundle.start(Bundle.START_TRANSIENT);
    }

    if (getPreferenceStore().getBoolean(PreferenceConstants.ENABLED)) {
      startServlet();
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

  public static ImageDescriptor getImageDescriptor(String path) {
    return imageDescriptorFromPlugin(PLUGIN_ID, path);
  }
  
  private void initializeValues() {
    final IPreferenceStore preferenceStore = getPreferenceStore();
    preferenceStore.setDefault(PORT, DEFAULT_PORT);
    preferenceStore.setDefault(ENABLED, DEFAULT_ENABLED);
  }
  
  private void startServlet() {
    final int portNo = getPreferenceStore().getInt(PreferenceConstants.PORT);

    final Dictionary<String, Comparable<?>> settings = new Hashtable<>();
    settings.put("http.enabled", Boolean.TRUE);
    settings.put("http.port", portNo);
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
