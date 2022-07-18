/**
 * 
 */
package net.sf.JRecord.Types;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * Type Char
 * <p>This class is the interface between the raw data in the file
 * and what is to be displayed on the screen for Character Strings
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypeChar extends BaseType {

	/**
	 * @param leftJustified
	 */
	public TypeChar(boolean leftJustified) {
		super(leftJustified);
	}

	   /**
     * Type Char
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Character Strings.
     * <p>It also acts as the base class to other Text base Types
     *
     * @param leftJustified left justified option
     */
    public TypeChar(final boolean leftJustified, boolean padByteOveride) {
        this(leftJustified, padByteOveride, false, false, true);

//        leftJust = leftJustified;
//        binary = false;
//        hasPadByteOveride = padByteOveride;
//        this.trim = false;
    }

    /**
     * Type Char
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Character Strings
     *
     * @param leftJustified left justified option
     * @param binaryField wether this is a binary field
     */
    public TypeChar(final boolean leftJustified, final boolean binaryField, final boolean num) {
    	super(leftJustified, false, binaryField, num, true);
    }


	/**
	 * @param leftJustified
	 * @param padByteOveride
	 * @param binaryField
	 * @param num
	 * @param trim
	 */
	public TypeChar(boolean leftJustified, boolean padByteOveride, boolean binaryField, boolean num, boolean trim) {
		super(leftJustified, padByteOveride, binaryField, num, trim);
	}

	@Override
	public boolean isValid(IFieldDetail fldDef, String value) {
		if (value == null) { return false; }
		int min = ' ';
		for (int i = 0; i < value.length(); i++) {
			int charAt = value.charAt(i);
			if (charAt < min) {
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean isValid(int pos, IFieldDetail fldDef, byte[] line) {
		if (line == null) { return false; }
		
		int end = Math.min(getFieldEnd(pos, fldDef, line), line.length - 1);
		pos -= 1;
		int min = Conversion.isEbcidic(fldDef.getFontName()) ? 0x40 : ' ';
		
		for (int i = pos; i < end; i++) {
			if (line[i] >= 0 && line[i] < min) {
				return false;
			}
		}
		
		return true;
	}

	
}
