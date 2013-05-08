package net.sf.JRecord.External.Def;


/**
 * This interface describes a class that will convert a Type/Format from the
 * external String representation to the internal integer value
 *
 *  @author Bruce Martin
 *
 */
public interface AbstractConversion {

	public static int USE_DEFAULT_IDX = -121;

	/**
	 * Convert a String to a Type value
	 * @param idx db index
	 * @param type Type (String)
	 *
	 * @return integer type value
	 */
	public abstract int getType(int idx, String type);


	/**
	 * Convert a String to a Formay value
	 * @param idx db index
	 * @param format Format (String)
	 *
	 * @return format
	 */
	public abstract int getFormat(int idx, String format);


	/**
	 * Convert type to a string
	 * @param idx db index
	 * @param type type Id
	 * @return Type as a String
	 */
	public abstract String getTypeAsString(int idx, int type);

	/**
	 *
	 * @param idx db index
	 * @param type type Id
	 * @return wether it is valid
	 */
	public abstract boolean isValid(int idx, int type);

	/**
	 * Convert format to a string
	 * @param idx db index
	 * @return format as a String
	 */
	public abstract String getFormatAsString(int idx, int format);


}
