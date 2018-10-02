package net.sf.JRecord.detailsBasic;

import java.util.Arrays;

import net.sf.JRecord.Common.Conversion;

/**
 * This class represents one Special Csv Character
 * (Csv Delimiter). You can get<ul>
 *  <li>the JRecord/Record editor definition (i.e. \u0009 or , or x'01'
 *  <li>The Character value/values
 *  <li>The byte value/values
 * </ul>
 * 
 * @author bruce
 *
 */
public final class CsvCharDetails {
//	public static final CsvCharDetails DEFAULT_DELIMITER =  newDelimDefinition("\t", "");
	
	private static final byte[] EMPTY_BYTES = new byte[0];
	private static final char[] EMPTY_CHARS = new char[0];
	
	public static final CsvCharDetails COMMA_DELIMITER = new CsvCharDetails(",", "", '\t'); 
	public static final CsvCharDetails TAB_DELIMITER = new CsvCharDetails("\t", "", '\t'); 
	public static final CsvCharDetails DEFAULT_QUOTE = new CsvCharDetails("\"", "", '\t');
	public static final CsvCharDetails DEFAULT_DELIMITER = new CsvCharDetails("\t", "", '\t'); 
	public static final CsvCharDetails SINGLE_QUOTE = new CsvCharDetails("'", "", '"');
	public static final CsvCharDetails DOUBLE_QUOTE = new CsvCharDetails("\"", "", '"');
	public static final CsvCharDetails EMPTY_QUOTE = new CsvCharDetails();
	
	
	public static CsvCharDetails newDelimDefinition(String rawValue, String font) {
		if (font != null && font.length() == 0) {
			if ("\t".equals(rawValue)) { return TAB_DELIMITER; }
			if (",".equals(rawValue)) { return COMMA_DELIMITER; }
		}
		if (rawValue == null || rawValue.length() == 0) { return EMPTY_QUOTE; }
		return new CsvCharDetails(rawValue, font, '\t');
	}
	public static CsvCharDetails newQuoteDefinition(String rawValue, String font) {
		if (font != null && font.length() == 0) {
			if ("'".equals(rawValue)) { return SINGLE_QUOTE; }
			if ("\"".equals(rawValue)) { return DOUBLE_QUOTE; }
		}
		if (rawValue == null || rawValue.length() == 0) { return EMPTY_QUOTE; }
		return new CsvCharDetails(rawValue, font, '\t');
	}
	
	public static CsvCharDetails newDef(byte b, String font) {
		return new CsvCharDetails(b, font);
	}

	private final String definition,  strValue, font;
	private final byte[] bytes;
	private final char[] chars;
	private final boolean bin;
	
	private CsvCharDetails(String rawValue, String font, char defaultCh) {
		this.definition = "\t".equals(rawValue) ? "\\t" : rawValue ;
		this.font = font;
		//this.fontname = font;
		
		bytes = Conversion.getCsvDelimBytes(rawValue, font, defaultCh);	
		chars = Conversion.decodeChar(rawValue, font, defaultCh);
		strValue = new String(chars);
		bin = Conversion.isHexDefinition(rawValue);
	}
	
	private CsvCharDetails(byte b, String font) {
		this.font = font;
		//this.fontname = font;
		
		bytes = new byte[]{b};	
		strValue = Conversion.toString(bytes, font);
		chars = strValue == null || strValue.length() == 0 ? new char[0] : strValue.toCharArray();
		this.definition = "\t".equals(strValue) ? "\\t" : strValue ;
		boolean tbin = true;
		if (chars.length == 1 && chars[0] >= 32 && chars[0] <128) {
			tbin = false;
		}
		bin = tbin;
	}


	private CsvCharDetails() {
		this.definition = "" ;
		this.font = "";
		//this.fontname = font;
		
		bytes = EMPTY_BYTES;	
		chars = EMPTY_CHARS;
		strValue = "";
		bin = false;
	}
	
	public boolean isBin() {
		return bin;
	}
	
	public final byte[] asBytes() {
		return bytes;
	}
	
	
	public final byte asByte() {
		return bytes[0];
	}
	
	
	public final char asChar() {
		return chars[0];
	}
	
	
	public final char[] asChars() {
		return chars;
	}

	
	public String asString() {
		return strValue;
	}
	
	/**
	 * @deprecated for use in JRecord/RecordEditor only. This method can not <b>relied upon</b>.
	 */
	public String getFont() {
		return font;
	}
	
	public String jrDefinition() {
		return definition;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof CsvCharDetails) {
			CsvCharDetails cmp = (CsvCharDetails) obj;
			return this == cmp
				|| definition.equals(cmp.definition)
				|| ((!bin) && strValue.equals(cmp.strValue))
				|| ((bin) && Arrays.equals(bytes, cmp.bytes));
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return 
				  (definition == null ? 0                : definition.hashCode())
				+ (bin                ? bytes.hashCode() : strValue.hashCode());
	}
}
