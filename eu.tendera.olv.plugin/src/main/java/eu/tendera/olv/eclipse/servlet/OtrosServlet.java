package eu.tendera.olv.eclipse.servlet;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;

import eu.tendera.olv.eclipse.fileutils.FileUtils;
import eu.tendera.olv.eclipse.fileutils.FileUtilsImpl;
import eu.tendera.olv.eclipse.fileutils.SourceLocation;

@WebServlet(urlPatterns = { "/*" })
public class OtrosServlet extends HttpServlet {

  private static final long serialVersionUID = 717006710949328871L;
  FileUtils fileUtils;

  enum Operation {
    form, test, jump, content
  }

  @Override
  public void init() throws ServletException {
    fileUtils = new FileUtilsImpl();
  }

  protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    response.setContentType("text/html");
    response.addHeader("ide", "eclipse");

    String operationString = getParameter(request, "operation", "o", "form");
    Operation operation = Operation.valueOf(operationString);

    switch (operation) {
      case form:
        form(response);
        return;
      case jump:
        jump(request, response, false);
        return;
      case test:
        jump(request, response, true);
        return;
      case content:
        content(request, response);
        return;
      default:
        break;
    }
    error(response, "Unexpected operation");
  }

  private void content(HttpServletRequest request, HttpServletResponse response) throws IOException {
    String packageName = getParameter(request, "p", "packageName");
    String fileName = getParameter(request, "f", "fileName");
    String className = getParameter(request, "c", "className");
    int lineNumber = parseInt(getParameter(request, "l", "lineNumber"), 0);
    String project = request.getParameter("project");
    String module = request.getParameter("module");
    SourceLocation location;
    if (packageName != null && fileName != null) {
      location = new SourceLocation(packageName, fileName, lineNumber, project, module);
    } else {
      if (className != null) {
        location = new SourceLocation(className);
      } else {
        error(response, "either (packageName,fileName) or (className) is required");
        return;
      }
    }
    String content = fileUtils.getContent(location);

    if (content.length() > 0) {
      response.setStatus(HttpServletResponse.SC_OK);
      response.addHeader("line", Integer.toString(location.getLineNumber()));
      response.getWriter().append(content);
    } else {
      response.setStatus(HttpServletResponse.SC_NOT_FOUND);
    }
  }

  private static String getParameter(HttpServletRequest request, String shortName, String longName) {
    String value = request.getParameter(longName);
    if (value == null) {
      value = request.getParameter(shortName);
    }
    return value;
  }

  private static String getParameter(HttpServletRequest request, String shortName, String longName, String defaultValue) {
    String value = getParameter(request, shortName, longName);
    return (value != null) ? value : defaultValue;
  }

  private void jump(HttpServletRequest request, HttpServletResponse response, boolean test) throws IOException {
    String packageName = getParameter(request, "p", "packageName");
    String fileName = getParameter(request, "f", "fileName");
    String className = getParameter(request, "c", "className");
    int lineNumber = parseInt(getParameter(request, "l", "lineNumber"), 0);
    String project = request.getParameter("project");
    String module = request.getParameter("module");
    SourceLocation location;
    if (packageName != null && fileName != null) {
      location = new SourceLocation(packageName, fileName, lineNumber, project, module);
    } else {
      if (className != null) {
        location = new SourceLocation(className);
      } else {
        error(response, "either (packageName,fileName) or (className) is required");
        return;
      }
    }
    boolean ok;
    if (test) {
      ok = fileUtils.isReachable(location);
    } else {
      ok = fileUtils.jumpToLocation(location);
    }
    if (ok) {
      response.setStatus(HttpServletResponse.SC_OK);
      if (test) {
        response.getWriter().println("OK, found " + location);
      } else {
        response.getWriter().println("OK, jumped to " + location);
      }
    } else {
      response.setStatus(HttpServletResponse.SC_NOT_FOUND);
      response.getWriter().println("Not found: " + location);
    }
  }

  private void form(HttpServletResponse response) throws IOException {
    IOUtils.copy(getClass().getClassLoader().getResourceAsStream("form.html"), response.getOutputStream());
    response.setStatus(HttpServletResponse.SC_OK);
  }

  private int parseInt(String value, int defaultValue) {
    if (value == null) {
      return defaultValue;
    }
    try {
      return Integer.parseInt(value);
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }

  private void error(HttpServletResponse response, String message) throws IOException {
    response.getWriter().println(message);
    response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
  }

}
