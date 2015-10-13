/**
 * @Author Jean-Francois Gagnon
 * Created on 6/26/2006
 *
 * Purpose:
 *    Add support for Fujitsu Type Zoned Decimal
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface
 */
package net.sf.JRecord.Types;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * Fujitsu Type Zoned Decimal type.
 *
 * @author Jean-Francois Gagnon
 *
 */
public class TypeFjZoned extends TypeNum {

	private static int positiveFjDiff = '@' - '0';
	private static int negativeFjDiff = 'P' - '0';

    /**
     * Define Fujitsu Zoned Decimal Type
     *
     * <p>This class is the interface between the raw data in the file
     * and what is to be displayed on the screen for Fujitsu Zoned Decimal
     * fields.
     */
    public TypeFjZoned() {
        super(false, true, true, false, false, false);
    }


    /**
     * @see net.sf.JRecord.Types.Type#getField(byte[], int, net.sf.JRecord.Common.FieldDetail)
     */
    public Object getField(byte[] record,
            final int position,
			final IFieldDetail field) {
        return addDecimalPoint(
                	fromFjZoned(super.getFieldText(record, position, field)),
                	field.getDecimal());
    }


    /**
     * @see net.sf.JRecord.Types.Type#setField(byte[], int, net.sf.JRecord.Common.FieldDetail, java.lang.Object)
     */
    public byte[] setField(byte[] record,
            final int position,
			final IFieldDetail field,
			Object value) {

	    copyRightJust(record, formatValueForRecord(field, toNumberString(value)),
	            position - 1, field.getLen(),
	            "0", field.getFontName());
	    return record;
    }

    
	@Override
	public String formatValueForRecord(IFieldDetail field, String value) {
		String val = toFjZoned(checkValue(field, toNumberString(value)));
		if (field.isFixedFormat()) {
			return Conversion.padFront(val, field.getLen() - val.length(), '0');
		}
		return val;
	}


	/**
	 * Convert a num to a Fujitsu Zoned Number String
	 *
	 * @param num  Numeric string
	 *
	 * @return number-string
	 */
	private String toFjZoned(String num) {

		String ret;
		if (num == null || (ret = num.trim()).length() == 0 || ret.equals("-") || ret.equals("+")) {
			return "";
		}


        char lastChar = ret.substring(ret.length() - 1).charAt(0);

		if (num.startsWith("-")) {
            ret = ret.substring(1);

			if (lastChar < '0' || lastChar > '9') {
				// throw ...
			} else {
				lastChar = (char) (lastChar + negativeFjDiff);
			}

        } else {
            if (num.startsWith("+")) {
                ret = ret.substring(1);
            }

            if (lastChar < '0' || lastChar > '9') {
                // throw ...
            } else {
                lastChar = (char) (lastChar + positiveFjDiff);
            }

		}

        ret = ret.substring(0, ret.length() - 1) + lastChar;

		return ret;
	}

    /**
     * Convert a Fujitsu Zoned Number String to a number string
     *
     * @param numZoned Zoned Numeric string
     *
     * @return number-string
     */
    private String fromFjZoned(String numZoned) {
        String ret;
        String sign = "";
        char lastChar;

        if (numZoned == null || (ret = numZoned.trim()).length() == 0 || ret.equals("-")) {
            return "";
        }

        lastChar = ret.substring(ret.length() - 1).toUpperCase().charAt(0);

        switch (lastChar) {
            case '@':
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
            case 'G':
            case 'H':
            case 'I':
                lastChar = (char) (lastChar - positiveFjDiff);
            break;
            case 'P':
            case 'Q':
            case 'R':
            case 'S':
            case 'T':
            case 'U':
            case 'V':
            case 'W':
            case 'X':
            case 'Y':
                sign = "-";
                lastChar = (char) (lastChar - negativeFjDiff);
            default:
        }
        ret = sign + ret.substring(0, ret.length() - 1) + lastChar;

        return ret;
    }
}
