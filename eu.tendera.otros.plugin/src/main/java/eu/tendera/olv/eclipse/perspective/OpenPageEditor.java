package eu.tendera.olv.eclipse.perspective;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;

import eu.tendera.olv.eclipse.fileutils.SourceLocation;

@SuppressWarnings("restriction")
public class OpenPageEditor implements Runnable {

  private final SourceLocation location;
  private Boolean result;

  public OpenPageEditor(SourceLocation location) {
    this.location = location;
  }

  public Boolean getResult() {
    return result;
  }

  @Override
  public void run() {
    try {

      final IFile file = getIFile(location);

      if (file == null) {
        result = Boolean.FALSE;
        return;
      }

      if (PlatformUI.getWorkbench().getWorkbenchWindowCount() < 1) {
        result = Boolean.FALSE;
        return;
      }

      final IWorkbenchPage page = PlatformUI.getWorkbench().getWorkbenchWindows()[0].getActivePage();
      final IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(file.getName());

      final IEditorPart editor = page.openEditor(new FileEditorInput(file), desc.getId());

      revealLine(editor, location.getLineNumber());

      final PackageExplorerPart part = PackageExplorerPart.openInActivePerspective();

      file.refreshLocal(IResource.DEPTH_ZERO, null);

      part.selectAndReveal(file);
      result = Boolean.TRUE;
    } catch (CoreException e) {
      e.printStackTrace();
      result = Boolean.FALSE;
    }
  }

  private void revealLine(IEditorPart editorPart, int lineNumber) {
    if (!(editorPart instanceof ITextEditor) || lineNumber <= 0) {
      return;
    }
    final ITextEditor editor = (ITextEditor) editorPart;
    final IDocument document = editor.getDocumentProvider().getDocument(editor.getEditorInput());
    if (document != null) {
      try {
        // line count internally starts with 0, and not with 1 like in GUI
        IRegion lineInfo = document.getLineInformation(lineNumber - 1);
        editor.selectAndReveal(lineInfo.getOffset(), lineInfo.getLength());
      } catch (BadLocationException e) {
        // ignored because line number may not really exist in document, we
        // guess this...
      }
    }
  }

  public static IFile getIFile(SourceLocation location) {
    IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();

    for (IProject project : projects) {
      if (project.isOpen() && JavaProject.hasJavaNature(project)) {
        final IJavaProject javaProject = JavaCore.create(project);
        final String filePath = location.getPackageName().replace(".", File.separator);
        try {
          for (IPackageFragmentRoot fragmentRoot : javaProject.getAllPackageFragmentRoots()) {
            if (fragmentRoot.getElementType() == IJavaElement.PACKAGE_FRAGMENT_ROOT && !fragmentRoot.isArchive()) {

              final Path path = Paths.get(fragmentRoot.getResource().getProjectRelativePath().toOSString(), filePath,
                  location.getFileName());

              final IFile file = project.getFile(path.toString());

              if (file.exists()) {
                return file;
              }
            }
          }
        } catch (JavaModelException e) {
          MessageDialog.openError(null, "Error occurred", e.getMessage());
        }
      }
    }
    return null;
  }

}