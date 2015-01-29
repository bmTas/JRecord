/*
 * Created on 8/01/2005
 *
 */
package net.sf.JRecord.Log;

/**
 * A do nothing Log
 *
 * @author Bruce Martin
 *
 */
public class NullLog implements AbsSSLogger {

	/**
	 * @see net.sf.JRecord.Log#setReportLevel(int)
	 */
	public void setReportLevel(int level) {
	}

	/**
	 * @see net.sf.JRecord.Log#logException(int, java.lang.Exception)
	 */
	public void logException(int level, Exception ex) {
	}
	
	/**
	 * @see net.sf.JRecord.Log#logMsg(int, java.lang.String)
	 */
	public void logMsg(int level, String msg) {
	}
	
	public static AbsSSLogger getLog(AbsSSLogger log) {
		if (log == null) {
			log = new NullLog();
		}
		return log;
	}
}