/*
 * Created on 8/01/2005
 *
 */
package net.sf.JRecord.Log;

import java.io.IOException;

/**
 * A very simple logger that writes details to System.out
 *
 * @author Bruce Martin
 *
 */
public class AppendableLog implements AbsSSLogger {
	private final Appendable logStore;
	

	public AppendableLog(Appendable logStore) {
		super();
		this.logStore = logStore;
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

		try {
			logStore.append('\n');
			
			if (ex != null) {
				logStore.append('\n');;
				CommonCode.append(logStore, ex);
			}
		} catch (IOException e) {
			CommonCode.printAppendError(e, ex);
		}
	}
	/**
	 * @see net.sf.JRecord.Log#logMsg(int, java.lang.String)
	 */
	public void logMsg(int level, String msg) {

		if (msg == null) {
			msg = "null";
		}
		try {
			logStore.append('\n');
			logStore.append(msg);
			logStore.append('\n');;
		} catch (IOException e) { 
			System.err.println();
			System.err.println("Error in Appending: " + msg + ": ");
			e.printStackTrace();
			
		}
	}
}