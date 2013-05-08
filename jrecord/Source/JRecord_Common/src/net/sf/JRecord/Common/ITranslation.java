package net.sf.JRecord.Common;


/**
 * Purpose: provide basic Language Translation services
 *
 * @author Bruce Martin
 *
 */
public interface ITranslation {

	public static final int ST_MESSAGE = 2;
	public static final int ST_ERROR   = 14;

	public String convert(int type, String s);
	public String convert(String s);
	public String convert(String s, String defaultStr);
	public abstract String convert(int type, String s, Object[] params);
	public abstract String convert(int type, String s, String param);
}
