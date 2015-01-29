/*
 * Created on 8/01/2005
 *
 */
package net.sf.JRecord.Log;

import java.io.PrintStream;

/**
 * A very simple logger that writes details to System.out
 *
 * @author Bruce Martin
 *
 */
public class TextLog implements AbsSSLogger {
	
	private final PrintStream outStream;

	public TextLog() {
		this(System.err);
	}
	
	public TextLog(PrintStream outStream) {
		this.outStream = outStream;
	}

	/**
	 * @see net.sf.JRecord.Log#setReportLevel(int)
	 */
	public void setReportLevel(int level) {
	}

	/**
	 * @see net.sf.JRecord.Log#logException(int, java.lang.Exception)
	 */
	public void logException(int level, Exception ex) {

		outStream.println();
		
		if (ex != null) {
			outStream.println();
			ex.printStackTrace(outStream);
		}
	}
	/**
	 * @see net.sf.JRecord.Log#logMsg(int, java.lang.String)
	 */
	public void logMsg(int level, String msg) {

		outStream.println();
		outStream.println(msg);
	}
	
	public static AbsSSLogger getLog(AbsSSLogger log) {
		if (log == null) {
			log = new TextLog();
		}
		return log;
	}
}