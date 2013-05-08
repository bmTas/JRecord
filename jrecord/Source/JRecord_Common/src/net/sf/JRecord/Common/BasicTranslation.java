package net.sf.JRecord.Common;

import java.text.MessageFormat;


public class BasicTranslation implements ITranslation {

	private static ITranslation trans = new BasicTranslation();

	@Override
	public final String convert(int type, String s, String param) {
		return MessageFormat.format(convert(type, s), param);
	}

	@Override
	public final String convert(int type, String s, Object[] params) {
		return MessageFormat.format(convert(type, s), params);
	}

	@Override
	public final String convert(int type, String s) {
		if (s == null || "".equals(s)) return s;

		return convert(s);
	}


	/**
	 * @see net.sf.JRecord.Common.ITranslation#convert(java.lang.String)
	 */
	@Override
	public final String convert(String s) {
		return convert(s, s);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.ITranslation#convert(java.lang.String, java.lang.String)
	 */
	@Override
	public String convert(String s, String defaultStr) {
		return defaultStr;
	}

	/**
	 * @return the trans
	 */
	public static ITranslation getTrans() {
		return trans;
	}

	/**
	 * @param trans the trans to set
	 */
	public static void setTrans(ITranslation trans) {
		BasicTranslation.trans = trans;
	}



}
