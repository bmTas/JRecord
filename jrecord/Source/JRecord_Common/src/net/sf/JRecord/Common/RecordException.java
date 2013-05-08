/*
 * Created on 19/12/2004
 *
 */
package net.sf.JRecord.Common;

/**
 * Error class for Jrecord / RecordEditor
 *
 * @author Bruce Martin
 * @version 0.51
 */
@SuppressWarnings("serial")
public class RecordException extends Exception {


	/**
	 * @param msg Error Message
	 */
	public RecordException(final boolean x, final String msg) {
		super(msg);
	}

	/**
	 * @param msg Error Message
	 */
	public RecordException(final String msg) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg));
	}

	/**
	 * @param msg Error Message
	 */
	public RecordException(final String msg, String parm) {
		this(msg, new Object[] {parm});
	}

	/**
	 * @param msg Error Message
	 */
	public RecordException(final String msg, Object[] parms) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg, parms));
	}


	public RecordException(String msg, Throwable exception) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg), exception);
	}

	public RecordException(String msg, Object[] parms, Throwable exception) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg, parms), exception);
	}

}
