package eu.tendera.olv.eclipse.preferences;

import static eu.tendera.olv.eclipse.preferences.PreferenceConstants.ENABLED;
import static eu.tendera.olv.eclipse.preferences.PreferenceConstants.PORT;

import java.io.InputStream;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import eu.tendera.olv.eclipse.Activator;

public class OLVPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

  private IntegerFieldEditor portField;
  private ComboFieldEditor enabledField;

  public OLVPreferencePage() {
    super(GRID);
    setPreferenceStore(Activator.getDefault().getPreferenceStore());
    setDescription("OtrosLogViewer preferences - restart is required to apply the changes.");

    final InputStream imageStream = getClass().getClassLoader().getResourceAsStream("/images/logo16.png");

    final Image logo = new Image(Display.getDefault(), imageStream);

    setImageDescriptor(ImageDescriptor.createFromImage(logo));
  }

  @Override
  protected Control createContents(Composite parent) {
    final Control composite = super.createContents(parent);
    GridLayoutFactory.fillDefaults().numColumns(3).applyTo(getFieldEditorParent());
    return composite;
  }

  @Override
  protected void createFieldEditors() {
    portField = new IntegerFieldEditor(PORT, "Port number: ", getFieldEditorParent(), 5);
    portField.setValidRange(1, 65536);
    portField.setValidateStrategy(IntegerFieldEditor.VALIDATE_ON_FOCUS_LOST);
    portField.setErrorMessage(String.format("Port number is not valid valid (1-65536).", portField.getStringValue()));
    addField(portField);

    final Button button = new Button(getFieldEditorParent(), SWT.NONE);
    button.setText("text");
    button.setImage(PlatformUI.getWorkbench().getDisplay().getSystemImage(SWT.ICON_WARNING));
    GridDataFactory.fillDefaults().applyTo(button);

    final String[][] entries = new String[][] { //
    new String[] { "On", Boolean.TRUE.toString() }, //
        new String[] { "Off", Boolean.FALSE.toString() } };

    enabledField = new ComboFieldEditor(ENABLED, "Switch ON/OFF", entries, getFieldEditorParent());
    addField(enabledField);
  }

  @Override
  public void init(IWorkbench workbench) {
  }
}
