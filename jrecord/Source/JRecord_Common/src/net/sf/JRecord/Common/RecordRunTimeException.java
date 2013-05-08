package net.sf.JRecord.Common;

@SuppressWarnings("serial")
public class RecordRunTimeException extends RuntimeException {


	/**
	 * @param msg Error Message
	 */
	public RecordRunTimeException(final String msg) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg));
	}

	/**
	 * @param msg Error Message
	 */
	public RecordRunTimeException(final String msg, String parm) {
		this(msg, new Object[] {parm});
	}

	/**
	 * @param msg Error Message
	 */
	public RecordRunTimeException(final String msg, Object[] parms) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg, parms));
	}


	public RecordRunTimeException(String msg, Throwable exception) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg), exception);
	}

	public RecordRunTimeException(String msg, String parm, Throwable exception) {
		super(BasicTranslation.getTrans().convert(BasicTranslation.ST_ERROR, msg, parm), exception);
	}

}
