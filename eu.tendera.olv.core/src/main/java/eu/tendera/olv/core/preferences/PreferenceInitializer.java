package eu.tendera.olv.core.preferences;

import static eu.tendera.olv.core.preferences.PreferenceConstants.DEFAULT_ENABLED;
import static eu.tendera.olv.core.preferences.PreferenceConstants.DEFAULT_PORT;
import static eu.tendera.olv.core.preferences.PreferenceConstants.ENABLED;
import static eu.tendera.olv.core.preferences.PreferenceConstants.PORT;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import eu.tendera.olv.core.Activator;

public class PreferenceInitializer extends AbstractPreferenceInitializer {

  @Override
  public void initializeDefaultPreferences() {
    IPreferenceStore store = Activator.getDefault().getPreferenceStore();
    store.setDefault(PORT, DEFAULT_PORT);
    store.setDefault(ENABLED, DEFAULT_ENABLED);
  }

}