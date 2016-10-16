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
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.Types;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.text.NumberFormat;
import java.util.Arrays;

import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.Common.CommonBits;
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

//    private static final String STRING_NULL_VALUE = (String) CommonBits.NULL_VALUE;
//	private static final int BASE_10 = 10;
	private final boolean couldBeHexZero;
    private final boolean adjustTheDecimal;
    protected final boolean couldBeEmpty;
    private boolean couldBeLong = true;
    private final boolean positive;
    private boolean usePositiveSign = false;
    private String padChar = " ";
    private final char decimalPoint;

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
    	this(typeId, isPositive, '.');
    }

    /**
     *
     * @param typeId
     * @param isPositive
     */
    protected TypeNum(final int typeId, final boolean isPositive, char decimalPoint) {
        super(typeId == Type.ftNumLeftJustified, false, true);
        
        boolean adjDecimal = false;
        this.couldBeHexZero = false;
        setNumeric(true);
        positive = isPositive;
        this.decimalPoint = decimalPoint;

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
        	adjDecimal = true;
        	padChar = "0";
        	break;
        case Type.ftNumCommaDecimal:
        case Type.ftNumCommaDecimalPositive:
        case Type.ftNumZeroPadded:
        case Type.ftNumZeroPaddedPositive:
            padChar = "0";
        }
        adjustTheDecimal = adjDecimal;
        couldBeEmpty = false;

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
            		  final boolean binary,
            		  final boolean couldBeHexHero,
            		  final boolean numCouldBeEmpty) {
        super(leftJustified, binary, true);
        adjustTheDecimal = adjustDecimal;
        couldBeLong = couldBeALong;
        positive = isPositive;
        this.couldBeHexZero = couldBeHexHero;
        this.couldBeEmpty = numCouldBeEmpty;
        this.decimalPoint = '.';
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
		String s = "";

		if (! isHexZero(record, position, currField.getLen())) {
			s =  super.getField(record, position, currField).toString();
		}

	    return addDecimalPoint(s, currField.getDecimal());
	}


//	public final boolean isDefined(final byte[] record, IFieldDetail currField) {
//		return record.length >= currField.getLen() 
//			&&  ((couldBeHexZero) || (! isHexZero(record, currField.getPos(), currField.getLen())));
//	}
	
	public final boolean isDefined(final AbstractIndexedLine line, byte[] record, IFieldDetail currField) {
		int pos = currField.calculateActualPosition(line);
		return record.length >= pos + currField.getLen() 
			&&  ((couldBeHexZero) || (! isHexZero(record, pos, currField.getLen())));
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

		if (((decimal != 0)
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

		    if (decimal > 0) {	    	
			    if (s.length() <= decimal) {
			        StringBuilder b = new StringBuilder();
			        char[] z = new char[decimal - s.length() + 1];
					Arrays.fill(z, '0');
			        s = b.append(z).append(s).toString();
			    }
			    len = s.length();

			    s = sign + Conversion.numTrim(s.substring(0, len - decimal))
			      + Conversion.getDecimalchar() + s.substring(len - decimal);
		    } else {
		    	StringBuilder b = new StringBuilder(s);
		        char[] z = new char[-decimal];
		        Arrays.fill(z, '0');
		        s =  Conversion.numTrim(b.append(z).toString());
		    }
		} else if (s.length() > 0) {
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
	@Override
	public byte[] setField(byte[] record,
	        final int position,
			final IFieldDetail field,
			Object value) {
		return setFieldToVal(record, position, field, checkValue(field, toNumberString(value)));
	}


	protected final byte[] setFieldToVal(byte[] record,
	        final int position,
			final IFieldDetail field,
			String val) {

	    int len = field.getLen();
	    int pos = position - 1;
	    String font = field.getFontName();

	    if (val != null && val.length() > 0) {
	    	String v = Conversion.numTrim(val, decimalPoint);
	    	if (( v.length() == 0 || v.charAt(0) == '.' || v.charAt(0) == ',' || v.charAt(0) == decimalPoint)) {
	    		v = '0' + v;
	    	}
	    	val = v;
	    }
	    
	    checkCharNumLength(val, len);

	    if (padChar.equals("0") && val.startsWith("-")) {
	        copyRightJust(record, val.substring(1), pos, len, "0", font);
	        record[pos] = Conversion.getBytes("-", font)[0];
	    } else if (padChar.equals("0") && usePositiveSign && val.length() < len) {
	    	if (val.startsWith("+")) {
	    		val = val.substring(1);
	    	}
	        copyRightJust(record, val, pos, len, "0", font);
	        record[pos] = Conversion.getBytes("+", font)[0];
	    } else if (typeIdentifier == Type.ftNumLeftJustified) {
			System.arraycopy(getBytes(val, font), 0, record, pos, val.length());
			padWith(record, pos + val.length(), len - val.length(), " ", font);
	    } else if (padChar.equals(" ") && usePositiveSign) {
	    	if (!(val.startsWith("+") || val.startsWith("-") ||  val.length() >= len)) {
	    		val = "+" + val;
	    	}
	        copyRightJust(record, val, pos, len, " ", font);
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
	 */
	public String formatValueForRecord(IFieldDetail field, String val) {
		String ret = checkValue(field, val);
		if (isBinary()) return ret;

		if (field.getLen() < 0) {
			if (usePositiveSign && (! ret.startsWith("-"))  && (! ret.startsWith("+"))) {
				ret = "+" + ret;
			}
			return ret;
		}
		int diff = ret.length() - Math.max(0, field.getLen());
		
		if (padChar.equals("0") && ret.startsWith("-")) {
			ret = padStr(ret.substring(1), field.getLen(), '-', '0'); 
		} else if (diff == 0) {
			
		} else if (diff > 0) {
			if (!ret.startsWith("-")) {
//				ret = '-' + ret.substring(diff + 1);
//			} else {
				ret = ret.substring(diff);
			}
	    } else if (padChar.equals("0")) {
	    	char sign = '0';
	    	if (usePositiveSign) {
	    		sign = '+';
	    	}
	    			
			ret = padStr(ret, field.getLen(), sign, '0'); 
	    } else if (typeIdentifier == Type.ftNumLeftJustified) {
	    	ret = new StringBuilder(ret).append(Conversion.getCharArray(-diff, ' ')).toString();
	    } else if (padChar.equals(" ") && usePositiveSign && (! ret.startsWith("+")) && (! ret.startsWith("-"))) {
	    	StringBuilder b =  new StringBuilder();
	    	if (diff < -1) {
	    		b.append(Conversion.getCharArray(-diff-1, ' '));
	    	}
	    	ret = b.append('+').append(ret).toString();
	    } else {
	    	ret = new StringBuilder().append(Conversion.getCharArray(-diff, ' ')).append(ret).toString();
	    }
		
		return ret;
	}
	
//	protected String padFront(String val, int size, char ch) {
//		return new StringBuilder()
//						.append(getCharArray(size, '0'))
//						.append(val)
//					.toString();
//	}
//	
//	protected final char[] getCharArray(int size, char ch) {
//		char[] c = new char[size];
//    	Arrays.fill(c, ch);
//    	return c;
//	}
	
	private String padStr(String s, int len, char firstCh, char padCh) {
		char[] c = new char[len];
		int toIndex = 0;

		c[0] = firstCh;
		if (len > s.length()) {
			toIndex = len - s.length();
			Arrays.fill(c, 1, toIndex, padCh);
		}
		System.arraycopy(s.toCharArray(), 0, c, toIndex, s.length());
		return new String(c);
	}
	
	
	public final String checkValue(IFieldDetail field, String val)
				throws RecordException {


		if (couldBeEmpty && (val == null || val.trim().length() == 0)) {
			return "";
		}
		if (val == null || val == CommonBits.NULL_VALUE) {
			val = "0";
		}
	    int decimal = field.getDecimal();
	    
	   
		if (decimal == 0 && couldBeLong && (val.indexOf('e') < 0 && val.indexOf('E') < 0 )) {
	        try {
	            val = val.trim();
	            if (val.startsWith("+")) {
	                val = val.substring(1);
	            }
	            if (val.lastIndexOf('.') >= 0) {
	            	//val = new BigDecimal(val).toString();
	            	
	            	val = val.substring(0, val.lastIndexOf('.'));
	            }
	            new BigInteger(val);
	        } catch (final Exception ex) {
	        	ex.printStackTrace();
	            throw new RecordException(field.getName() + " Invalid Integer :" + val + ": ~ " + ex);
	        }
	    } else {
	        try {
	        	// TODO Introduce localisation !!!!
	            BigDecimal decimalVal = new BigDecimal(Conversion.numTrim(val));

	            NumberFormat nf = getNumberFormat();
	            nf.setGroupingUsed(false);
	            if ((decimal != 0 && adjustTheDecimal) || decimal < 0) {
	                decimalVal = adjustForDecimal(field, decimalVal);
//	                decimalVal = decimalVal.multiply(new BigDecimal(
//	                        java.lang.Math.pow(BASE_10, field.getDecimal())));
	                nf.setMaximumFractionDigits(0);
	            } else {
	                nf.setMaximumFractionDigits(decimal);
	                nf.setMinimumFractionDigits(decimal);
	            }
	            nf.setRoundingMode(RoundingMode.DOWN);
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
	
	public final String toNumberString(Object value) {
		if (value == null || value == CommonBits.NULL_VALUE) {
			return "0";
		}
		return value.toString();
	}
	
	public boolean hasFloatingDecimal() {
		return false;
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
		return adjustForDecimal(field, new BigDecimal(Conversion.numTrim(val)));
	}

	/**
	 * Adjust Big decimal for decimal value 
	 * @param field field Definition
	 * @param decimalVal decimal value to be adjusted
	 * @return adjusted decimal value
	 */
	protected BigDecimal adjustForDecimal(IFieldDetail field, BigDecimal decimalVal) {
	    if ((field.getDecimal() != 0) && adjustTheDecimal) {
//	        decimalVal = decimalVal.setScale(decimalVal.scale() + field.getDecimal());
//	        decimalVal = decimalVal.multiply(new BigDecimal(BigInteger.ONE, field.getDecimal()));
//		        java.lang.Math.pow(BASE_10, field.getDecimal())));
//	        decimalVal = decimalVal.multiply(new BigDecimal(
//			        java.lang.Math.pow(BASE_10, field.getDecimal())));
	        
	        decimalVal = decimalVal.scaleByPowerOfTen(field.getDecimal());
	    }

	    return decimalVal;
	}


	/**
	 * Check the length of a field
	 *
	 * @param val value to check
	 * @param length length to check it against
	 * 
	 */
	private void checkCharNumLength(String val, int length) throws RecordException {

	    if (val.length() > length) {
	        throw new RecordException(
	        		"Value {0} is to big !! {1} > {2}",
	        		new Object[] {val, val.length(), length});
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

	protected final BigInteger formatAsBigInt(final IFieldDetail field, Object value) {
    	BigInteger v;

        if (value == null || value == CommonBits.NULL_VALUE) {
        	v = BigInteger.ZERO ;
        } else if (field.getDecimal() == 0 && value instanceof BigInteger) {
        	v = checkPositive((BigInteger) value);
        } else if (value instanceof BigDecimal) {
        	v = checkPositive(adjustForDecimal(field, (BigDecimal) value).toBigInteger());
        } else {
        	 String val = toNumberString(value);

//        	formatValueForRecord(field, val); // This is for validation purposes only
//        									  // the return value is deliberately not used
//        	v = getBigDecimal(field, val).toBigInteger();
        	v = new BigInteger(checkValue(field, val));
        }
        
        return v;
    }

	private BigInteger checkPositive(BigInteger v) throws RecordException {
		if (isPositive() && v.compareTo(BigInteger.ZERO) < 0) {
			throw new RecordException("Only positive numbers are allowed");
		}
		return v;
	}
}
