package eu.tendera.otros.eclipse.fileutils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import eu.tendera.otros.eclipse.perspective.OpenPageEditor;


public final class FileUtilsImpl implements FileUtils {

  @Override
  public String getContent(SourceLocation location) {

    if (!isReachable(location)) {
      return null;
    }
    
    final IFile file = OpenPageEditor.getIFile(location);
    if (file == null) {
      return null;
    }

    try {
      return readFileSelectedLines(location.getLineNumber(), file.getContents());
    } catch (CoreException e) {
      MessageDialog.openError(null, "Error occurred", e.getLocalizedMessage());
    }
    return null;

  }

  @Override
  public boolean isReachable(SourceLocation location) {
    return OpenPageEditor.getIFile(location) != null;
  }

  @Override
  public boolean jumpToLocation(final SourceLocation location) {

    final OpenPageEditor runnable = new OpenPageEditor(location);

    Display.getDefault().syncExec(runnable);

    return runnable.getResult();
  }

  

  /**
   * Get line number with context, line numbers with ':' is added before line
   *
   * @param lineNumber
   * @param inputStream
   */
  private String readFileSelectedLines(int lineNumber, InputStream inputStream) {
    int lineNr = 1;
    final StringBuilder stringBuilder = new StringBuilder();
    try (final BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
      String s;
      while ((s = reader.readLine()) != null && lineNr <= lineNumber) {
        if (lineNr++ > lineNumber - 3) {
          stringBuilder //
              .append(lineNr) //
              .append(": ") //
              .append(s) //
              .append(System.lineSeparator());
        }
      }
    } catch (IOException e) {

    }
    return stringBuilder.toString();
  }
}
