/*
 * @Author Bruce Martin
 * Created on 5/09/2005
 *
 * Purpose:
 * This class is the base for all numeric-Type classes +
 * it handles all character based numeric types (apart from zoned)
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - Added Base_10 constant
 *   - Added isPositive method
 *   - fixed bug for left justified numbers
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Starting to seperate the Record package out from the RecordEditor
 *     so that it can be used seperately. So classes have been moved
 *     to the record package (ie RecordException + new Constant interface
 *   - Added new getFieldType method (for Sroting / Jasper interface).
 *
 */
package net.sf.JRecord.Types;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.NumberFormat;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;


/**
 * This class is the base for all numeric-Type classes +
 * it handles all character based numeric types (apart from zoned)
 *
 * @author Bruce Martin
 *
 * @version 0.55
 */
public class TypeNum extends TypeChar {

    private static final int BASE_10 = 10;
    private boolean adjustTheDecimal;
    private boolean couldBeLong = true;
    private final boolean positive;
    private boolean usePositiveSign = false;
    private String padChar = " ";

    private int typeIdentifier;




    /**
     * Character based numeric Numeric Types:
     * <ul>
     *   <li>ftNumLeftJustified
     *   <li>ftNumRightJustified
     *   <li>ftNumZeroPadded
     *   <li>ftAssumedDecimal
     * </ul>
     *
     * <p><b>Note:</b> A "Type" is the interface between the raw data
     * in the file and what is to be displayed on the screen. A "Type"
     * class needs to perform all conversions file and the screen.
     *
     * @param typeId Type Identier values are:
     * <ul>
     *   <li>ftNumLeftJustified
     *   <li>ftNumRightJustified
     *   <li>ftNumZeroPadded
     *   <li>ftAssumedDecimal
     * </ul>
     */
    public TypeNum(final int typeId) {
    	this(typeId, false);
    }

    /**
     *
     * @param typeId
     * @param isPositive
     */
    public TypeNum(final int typeId, final boolean isPositive) {
        super(typeId == Type.ftNumLeftJustified, false, true);

        setNumeric(true);
        positive = isPositive;

        typeIdentifier = typeId;

        switch (typeId) {
        case Type.ftNumRightJustifiedPN:
        case Type.ftNumRightJustCommaDpPN:
        	usePositiveSign = true;
        	break;
        case Type.ftNumZeroPaddedPN:
        case Type.ftNumCommaDecimalPN:
        	usePositiveSign = true;
        	padChar = "0";
        	break;
        case Type.ftAssumedDecimal:
        case Type.ftAssumedDecimalPositive:
        	adjustTheDecimal = true;
        	padChar = "0";
        	break;
        case Type.ftNumCommaDecimal:
        case Type.ftNumCommaDecimalPositive:
        case Type.ftNumZeroPadded:
        case Type.ftNumZeroPaddedPositive:
            padChar = "0";
        }

        couldBeLong = typeId != Type.ftNumLeftJustified;
    }


    /**
     * Character based numeric Numeric Types:
     * <ul>
     *   <li>ftNumLeftJustified
     *   <li>ftNumRightJustified
     *   <li>ftNumZeroPadded
     *   <li>ftAssumedDecimal
     * </ul>
     *
     * <p><b>Note:</b> A "Type" is the interface between the raw data
     * in the file and what is to be displayed on the screen. A "Type"
     * class needs to perform all conversions file and the screen.
     *
     * @param leftJustified left justified field
     * @param adjustDecimal adjust the decimal places
     * @param couldBeALong could be a Long (ie integer)
     * @param isPositive wether this is a positive numeric field
     * @param binary     wether this is a binary field
     */
    protected TypeNum(final boolean leftJustified,
            		  final boolean adjustDecimal,
            		  final boolean couldBeALong,
            		  final boolean isPositive,
            		  final boolean binary) {
        super(leftJustified, binary, true);
        adjustTheDecimal = adjustDecimal;
        couldBeLong = couldBeALong;
        positive = isPositive;
    }



	/**
	 * Extracts a field out of a record
	 *
	 * @param record record which is to have the field extracted
	 * @param position position of the field in the record
	 * @param currField Field Details
	 *
	 * @return the request field (formated)
	 */
	public Object getField(final byte[] record,
	        final int position,
			final IFieldDetail currField) {
	    Object ret = super.getField(record, position, currField);

	    ret = addDecimalPoint(ret.toString(), currField.getDecimal());
	    return ret;
	}



	/**
	 * Add decimal point to string if necessary
	 *
	 * @param s raw value
	 * @param decimal number after decimal place
	 *
	 * @return Numeric string with decimal point added
	 */
	protected String addDecimalPoint(String s, int decimal) {
	    int len;
	    String sign = "";

		if (((decimal > 0)
		&& adjustTheDecimal
		&&  (! s.endsWith(" ")) && Conversion.isInt(s))) {
		    if (s.startsWith("-")) {
		        s = s.substring(1);
		        sign = "-";
		    } else {
		    	if (s.startsWith("+")) {
		    		s = s.substring(1);
		    	}
		    }

		    if (s.length() <= decimal) {
		        int i;
		        for (i = 0; i <= decimal; i++) {
		            s = "0" + s;
		        }
		    }
		    len = s.length();

		    s = sign + Conversion.numTrim(s.substring(0, len - decimal))
		      + Conversion.getDecimalchar() + s.substring(len - decimal);
		} else if (! s.equals("")) {
		    s = Conversion.numTrim(s);
		}
		return s;
	}



	/**
	 * Sets a field to a new value
	 *
	 * @param record record which is to be update
	 * @param position field start position
	 * @param field Field Details
	 * @param value new value
	 *
	 * @return update record
	 * @throws RecordException any error that occurs during the save
	 */
	public byte[] setField(byte[] record,
	        final int position,
			final IFieldDetail field,
			Object value)
	throws RecordException {

	    int len = field.getLen();
	    int pos = position - 1;
	    String font = field.getFontName();
	    String val  = formatValueForRecord(field, value.toString());

	    checkCharNumLength(val, len);

	    if (padChar.equals("0") && val.startsWith("-")) {
	        copyRightJust(record, val.substring(1), pos, len, "0", font);
	        record[pos] = '-';
	    } else if (padChar.equals("0") && usePositiveSign) {
	    	if (val.startsWith("+")) {
	    		val = val.substring(1);
	    	}
	        copyRightJust(record, val, pos, len, "0", font);
	        record[pos] = '+';
	    } else if (typeIdentifier == Type.ftNumLeftJustified) {
			System.arraycopy(getBytes(val, font), 0, record, pos, val.length());
			padWith(record, pos + val.length(), len - val.length(), " ", font);
	    } else {
	        copyRightJust(record, val, pos, len, padChar, font);
	    }

	    return record;
	}


	/**
	 * Format a value for storing in the record
	 *
	 * @param field field definition
	 * @param val value to be formated
	 *
	 * @return value value as it is store in the record
	 * @throws RecordException any conversion errors
	 */
	public String formatValueForRecord(IFieldDetail field, String val)
	throws RecordException {

	    if (field.getDecimal() == 0 && couldBeLong) {
	        try {
	            val = val.trim();
	            if (val.startsWith("+")) {
	                val = val.substring(1);
	            }
	            new BigInteger(val);
	        } catch (final Exception ex) {
	            throw new RecordException("Invalid Integer :" + val + ": ~ " + ex.getMessage());
	        }
	    } else {
	        try {
	        	// TODO Introduce localisation !!!!
	            BigDecimal decimalVal = new BigDecimal(Conversion.numTrim(val));

	            NumberFormat nf = getNumberFormat();
	            nf.setGroupingUsed(false);
	            if ((field.getDecimal() > 0) && adjustTheDecimal) {
	                decimalVal = decimalVal.multiply(new BigDecimal(
	                        java.lang.Math.pow(BASE_10, field.getDecimal())));
	                nf.setMaximumFractionDigits(0);
	            } else {
	                nf.setMaximumFractionDigits(field.getDecimal());
	                nf.setMinimumFractionDigits(field.getDecimal());
	            }
	            val = nf.format(decimalVal);
	        } catch (final Exception ex) {
	            throw new RecordException("Invalid Number: " + ex.getMessage());
	        }

	    }
	    if (positive && val.indexOf('-') >= 0) {
	        throw new RecordException("Only positive numbers are allowed");
	    }
	    return val;
	}

	protected NumberFormat getNumberFormat() {
		return Conversion.getNumberformat();
	}


	/**
	 * Convert String to BigDecimal
	 *
	 * @param field field definition
	 * @param val value to be converted
	 *
	 * @return valuer as a big integer
	 */
	protected BigDecimal getBigDecimal(IFieldDetail field, String val) {

	    BigDecimal decimalVal = new BigDecimal(Conversion.numTrim(val));

	    if ((field.getDecimal() > 0) && adjustTheDecimal) {
	        decimalVal = decimalVal.multiply(new BigDecimal(
		        java.lang.Math.pow(BASE_10, field.getDecimal())));
	    }

	    return decimalVal;
	}


	/**
	 * Check the length of a field
	 *
	 * @param val value to check
	 * @param length length to check it against
	 *
	 * @throws RecordException to big error
	 */
	private void checkCharNumLength(String val, int length) throws RecordException {

	    if (val.length() > length) {
	        throw new RecordException(
	        		"Value is to big !! {0} > {1}",
	        		new Object[] {val.length(), length});
	    }
	}


    /**
     * get the positive value. This attribute was originally setup for <b>internal</b> use
     * (with binary numbers). I will try and fix it for other types, but it might / might not
     * be set correctly.
     * Use it at you own risk !!
     * @return Returns the positive.
     */
    public boolean isPositive() {
        return positive;
    }

    /**
	 * @return the padChar
	 */
	public String getPadChar() {
		return padChar;
	}

	/**
     * @see net.sf.JRecord.Types.Type#getFieldType()
     */
    public int getFieldType() {
        return Type.NT_NUMBER;
    }


    protected long getRmComp(byte[] record, int position, int len) {
		long retL = 0;

		position = position - 1;
		if (record != null && record.length >= position) {
			int en = position + Math.min(len, record.length - position);

			for (int i = position; i < en; i++) {
				retL = record[i] + retL * 10;
			}

			if (len > record.length - position) {
				for (int i = len - record.length + position; i > 0; i--) {
					retL = retL * 10;
				}
			}
		}

		return retL;
    }

    protected byte[] setRmComp(byte[] record, int position, int len, long value) {

    	int ii;
 		for (int i = len - 1; i >= 0; i--) {
			ii = (int) value % 10;
			record[position + i - 1] = (byte) ii;
			value = value / 10;
		}

		return record;
    }
}
