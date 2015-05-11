package eu.tendera.olv.eclipse.fileutils;


public interface FileUtils {

	/**
	 * Returns content of location
	 * @param location
	 * @returns
	 */
	String getContent(SourceLocation location);

	/**
	 * Check if source location can be open in Eclipse
	 * @param location
	 * @return true if source can bee open in eclipse
	 */
	boolean isReachable(SourceLocation location);

	/**
	 * Open source location in IDE
	 * @param location
	 * @return location was opened
	 */
	boolean jumpToLocation(SourceLocation location);

}
