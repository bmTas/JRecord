/*
 * Created on 6/01/2005
 *
 */
package net.sf.JRecord.Log;

/**
 *
 * This is a definition of a Super Simple Logger
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - added SHOW option
 *
 * @author Bruce Martin
 */
public interface AbsSSLogger {

	public static final int TESTING = 10;
	public static final int WARNING = 20;
	public static final int LOG   = 29;
	public static final int ERROR   = 30;
	public static final int SHOW    = 30;

	/**
	 * Log a Error Message.
	 *
	 * @param level Error Level (ie Testing, warning etc)
	 * @param msg   Error Message
	 */
	public void logMsg(int level, String msg);

	/**
	 *
	 * @param level Error Level (ie Testing, warning etc)
	 * @param ex    Error Exception
	 */
	public void logException(int level, Exception ex);


	/**
	 * Sets the error level to be reportedon; Any Error above this level
	 * is reported on
	 *
	 * @param level Error Level (ie Testing, warning etc)
	 */
	public void setReportLevel(int level);
}
