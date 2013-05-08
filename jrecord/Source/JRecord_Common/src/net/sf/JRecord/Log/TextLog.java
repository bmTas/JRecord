/*
 * Created on 8/01/2005
 *
 */
package net.sf.JRecord.Log;

/**
 * A very simple logger that writes details to System.out
 *
 * @author Bruce Martin
 *
 */
public class TextLog implements AbsSSLogger {

	/**
	 * @see net.sf.JRecord.Log#setReportLevel(int)
	 */
	public void setReportLevel(int level) {
	}

	/**
	 * @see net.sf.JRecord.Log#logException(int, java.lang.Exception)
	 */
	public void logException(int level, Exception ex) {

		System.out.println();
		
		if (ex != null) {
			System.out.println();
			ex.printStackTrace();
		}
	}
	/**
	 * @see net.sf.JRecord.Log#logMsg(int, java.lang.String)
	 */
	public void logMsg(int level, String msg) {

		System.out.println();
		System.out.println(msg);
	}
}