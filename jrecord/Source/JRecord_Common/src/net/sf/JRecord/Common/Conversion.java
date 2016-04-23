/*
 * @Author Bruce Martin
 * Created on 11/06/2005
 *
 * Purpose: Various Conversion routines
 *
 * Changes
 * # Version 0.56 Bruce Martin 2007/01/16
 *   - corrected null checking in toZoned method
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
      
package net.sf.JRecord.Common;

import java.math.BigInteger;
import java.nio.charset.Charset;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Locale;




/**
 * This static class holds various conversion routines
 *
 * @author Bruce Martin
 *
 */
public final class Conversion {

	public static final HoldEbcidicFlag DEFAULT_CHARSET_DETAILS = new HoldEbcidicFlag("");
	private static String defaultSingleByteCharacterset = "";
	private static boolean alwaysUseDefaultSingByteCharset = false;

	private static HoldEbcidicFlag holdEbcidicFlag = DEFAULT_CHARSET_DETAILS;
	public static String DEFAULT_ASCII_CHARSET ;
	public static final boolean IS_DEFAULT_CHARSET_SINGLE_BYTE_EBCIDIC = DEFAULT_CHARSET_DETAILS.isSingleByteEbcidic;// isSingleByteEbcidicI("");
    private static final String VALUE_IS_TO_BIG_FOR_FIELD = "Value is to big for field {0} > {1} {2} ~ {3} {4}";
	private static final byte BYTE_NO_BIT_SET   =  0;
	private static final byte BYTE_ALL_BITS_SET = -1;

	private static int positiveDiff = 'A' - '1';
	private static int negativeDiff = 'J' - '1';
	
	private static char positive0EbcdicZoned = '{';
	private static char negative0EbcdicZoned = '}';

	//private static final DecimalFormatSymbols decSymbols = new DecimalFormatSymbols();
	private static final char decimalChar = '.';  //decSymbols.getDecimalSeparator();
	private static final NumberFormat numberFormat = NumberFormat.getNumberInstance(Locale.US);

	static {
		try {
			if (DEFAULT_CHARSET_DETAILS != null && DEFAULT_CHARSET_DETAILS.isEbcdic) {
				DEFAULT_ASCII_CHARSET = "cp1252";
				defaultSingleByteCharacterset = DEFAULT_CHARSET_DETAILS.charset;
				if (DEFAULT_CHARSET_DETAILS.isMultiByte) {
					defaultSingleByteCharacterset = "CP037";
				}
			} else {
				if (DEFAULT_CHARSET_DETAILS == null || DEFAULT_CHARSET_DETAILS.isMultiByte) {
					DEFAULT_ASCII_CHARSET = "cp1252";
					setDefaultSingleByteCharacterset("cp1252");
				} else {
					DEFAULT_ASCII_CHARSET = "";
					defaultSingleByteCharacterset = "";
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Conversion routines
	 *
	 */
	private Conversion() {

	}


	/**
	 * Converts a string to a byte array
	 *
	 * @param str String to be converted
	 * @param fontname Strings font
	 *
	 * @return equivalent byte array
	 */
	public static byte[] getBytes(String str, String fontname) {

	    if (str == null) {
	        return null;
	    } else if (fontname != null && fontname.length() > 0) {
	        try {
	            return str.getBytes(fontname);
	        } catch (Exception e) {
	        }
	    }
	    return str.getBytes();
	}

	/**
	 * Get a String Field
	 *
	 * @param record record to be updated
	 * @param start Field start
	 * @param fin Field end
	 * @param fontName font name being used
	 *
	 * @return Field Value
	 */
	public static String getString(final byte[] record,
	        				 final int start, final int fin,
	        				 final String fontName) {
		String s = "";

		if (fin - start <= 0) {
		} else if ( fontName == null || fontName.length() == 0) {
		    s = new String(record, start, fin - start);
		} else {
		    try {
		        s = new String(record, start, fin - start, fontName);
		    } catch (Exception e) {
		        //System.out.println("Conversion Error 1  " + e.getMessage() + " >> " + fontName);
		        s = new String(record, start, fin - start);
		    }
		}

		return s;
	}

	/**
	 *  Convert byte array to string
	 * @param record  record to be updated
	 * @param fontName font name being used
	 * @return record as string
	 */
	public final static String toString(final byte[] record,
			 final String fontName) {
		String s = "";

		try {
			if (fontName == null || fontName.length() == 0) {
			    s = new String(record);
			} else {
			    try {
			        s = new String(record, fontName);
			    } catch (Exception e) {
			        //System.out.println("Conversion Error 2 " + e.getMessage());
			        s = new String(record);
			    }
			}
		} catch (Exception e) {
		}

		return s;
	}


	/**
	 * Convert a Mainframe Zoned Number String to a number string
	 *
	 * @param numZoned Zoned Numeric string
	 *
	 * @return number-string
	 */
	public static String fromZoned(String numZoned) {
		String ret;
		String sign = "";
		char lastChar, ucLastChar;

		if (numZoned == null || ((ret = numZoned.trim()).length() == 0) || ret.equals("-")) {
			return "";
		}

		lastChar = ret.charAt(ret.length() - 1);
		ucLastChar = Character.toUpperCase(lastChar);

		
		switch (ucLastChar) {
//				case '}' : sign = "-";
//				case '{' :
//					lastChar = '0';
//				break;
		case 'A':
		case 'B':
		case 'C':
		case 'D':
		case 'E':
		case 'F':
		case 'G':
		case 'H':
		case 'I':
			lastChar = (char) (ucLastChar - positiveDiff);
			break;
		case 'J':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
		case 'P':
		case 'Q':
		case 'R':
			sign = "-";
			lastChar = (char) (ucLastChar - negativeDiff);
			break;
		default:
			if (lastChar == positive0EbcdicZoned) {
				lastChar = '0';
			} else if (lastChar == negative0EbcdicZoned) {
				lastChar = '0';
				sign = "-";
			}			
		}
		ret = sign + ret.substring(0, ret.length() - 1) + lastChar;

		return ret;
	}


	/**
	 * Get a Decimal field from an array of bytes
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Field start
	 * @param fin Field End
	 *
	 * @return Field Value (Decimal)
	 */
	public static String getDecimal(final byte[] record, final int start, final int fin) {
		int i;
		String s;
		StringBuffer ret = new StringBuffer("");
		int b;

		for (i = start; i < fin; i++) {
			b = toPostiveByte(record[i]);
			s = Integer.toHexString(b);
			if (s.length() == 1) {
				ret.append('0');
			}
			ret.append(s);

		}

		return ret.toString();
	}


	/**
	 * Get field Value as a Bit String
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Field start
	 * @param fin Field End
	 *
	 * @return Bit Fields Value
	 */
	public static String getBitField(final byte[] record, final int start, final int fin) {
		int i;
		StringBuffer ret = new StringBuffer("");
		String conv;

		for (i = start; i < fin; i++) {
		    conv = Integer.toBinaryString(toPostiveByte(record[i]));
		    if (conv.length() < 8) {
		        conv = "00000000".substring(conv.length()) + conv;
		        //System.out.println("==> " + conv);
		    }
			ret.append(conv);
		}

		return ret.toString();
	}


	/**
	 * Get a Postive Binary Integer (Intel Format)
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Field start
	 * @param fin Field End
	 *
	 * @return Positive Integer Field
	 */
	public static String getPostiveBinary(final byte[] record, final int start, final int fin) {
		int i;
		long l = 0;

		for (i = fin - 1; i >= start; i--) {
			l = l << 8;
			l |= toPostiveByte(record[i]);
		}

		return Long.toString(l);
	}


	/**
	 * Get a Binary Integer (Intel Format)
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Field start
	 * @param fin Field End
	 *
	 * @return Integer Field
	 */
	public static BigInteger getLittleEndianBigInt(final byte[] record, final int start, final int fin) {
		int len = fin - start;
		byte[] bytes = new byte[len];

		for (int i = 0; i < len; i++) {
			bytes[i] = record[start + len - i - 1];
		}

		return new BigInteger(bytes);
	}

	/**
	 * Get a Binary Integer (Intel Format)
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Field start
	 * @param fin Field End
	 *
	 * @return Integer Field
	 */
	public static String getBinaryInt(final byte[] record, final int start, final int fin) {
			return (getLittleEndianBigInt(record, start, fin)).toString();

//		long l = 0;
//		int len = fin - start;
//
//		//System.out.println(">> Get Len " + len + " : " + record[start] + " " + record[fin-1]);
//	    if (record[fin - 1] >= 0) {
//	    	return getPostiveBinary(record, start, fin);
//	    }
//
//	    switch (len) {
//	    	case 1:
//	    		l = record[start];
//	    	break;
//			case 2:
//			    byte[] rec = new byte[2];
//			    rec[0] = record[start + 1];
//			    rec[1] = record[start];
//			    l = (new BigInteger(rec)).longValue();
//			break;
//			case 4:
//				l = getBin(record, 0xFFFFFFFF, start, fin);
//			break;
//			case 8:
//				l = getBin(record, 0, start, fin);
//			break;
//			default :
//				return getPostiveBinary(record, start, fin);
//
//	    }
//
//		return Long.toString(l);
	}


	/**
	 * Converts a Mainframe Packed Decimal (Comp-3) field to String
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Start of field
	 * @param len Length of the field
	 *
	 * @return the value of a field
	 */
	public static String getMainframePackedDecimal(final byte[] record,
	        									   final int start,
	        									   final int len) {
	    String hex  = getDecimal(record, start, start + len);
	        //Long.toHexString(toBigInt(start, len).longValue());
	    String ret  = "";
	    String sign = "";

	    if (! "".equals(hex)) {
	        switch (hex.substring(hex.length() - 1).toLowerCase().charAt(0)) {
	            case 'd' : sign = "-";
	        	case 'a' :
	        	case 'b' :
	        	case 'c' :
	        	case 'e' :
	        	case 'f' :
	        	    ret = sign + hex.substring(0, hex.length() - 1);
	        	break;
	        	default:
	        	    ret = hex;
	        }
	    }

	    if ("".equals(ret)) {
	        ret = "0";
	    }

	    return ret;
	}


	/**
	 * Extract Field as a BigInteger
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Field start
	 * @param len Field length
	 *
	 * @return Field as a big Integer
	 */
	public static BigInteger getBigInt(final byte[] record, final int start, final int len) {
		byte[] bytes = new byte[len];

		System.arraycopy(record, start, bytes, 0, len);

		return new BigInteger(bytes);
	}




	/**
	 * Extract Field as a Positive BigInteger
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param start Field start
	 * @param len Field length
	 *
	 * @return Field as a big Integer
	 */
	public static BigInteger getPositiveBigInt(final byte[] record, final int start, final int len) {
		byte[] bytes = new byte[len + 1];
		bytes[0] = 0;

		System.arraycopy(record, start, bytes, 1, len);

		return new BigInteger(bytes);
	}



	/**
	 * Convert the requested field to a Long
	 *
	 * @param record Full record from which the value is to be extracted
	 * @param initial Initial Value
	 * @param start Field start
	 * @param fin Field End
	 *
	 * @return Field as a Long
	 */
	public static long getBin(byte[] record, long initial, final int start, final int fin) {
		int i;
		for (i = fin - 1; i >= start; i--) {
			initial <<= 8;
			initial |= toPostiveByte(record[i]);
		}
		return initial;
	}


	/**
	 * Convert a Byte (-128 .. 127) to a Postive Byte (0 .. 255)
	 * @param b input byte
	 *
	 * @return equivalent postive byte
	 */
	private static int toPostiveByte(byte b) {
		return (b) & 255;
//		if (b < 0) {
//			return 256 + b;
//		}
//		return b;
	}


	/**
	 * Convert a num to a Mainframe Zoned Number String
	 *
	 * @param num  Numeric string
	 *
	 * @return number-string
	 */
	public static String toZoned(String num) {
	    if (num == null) {
	        return "";
	    }
	    String ret = num.trim();

		if (num.equals("") || num.equals("-") || num.equals("+")) {
			// throw ...
			return "";
		}

		char lastChar = ret.substring(ret.length() - 1).charAt(0);
		//System.out.print(ret + " Char - " + lastChar);
		if (lastChar < '0' || lastChar > '9') {
		} else if (num.startsWith("-")) {
			if (lastChar == '0') {
				lastChar = negative0EbcdicZoned;
			} else {
				lastChar = (char) (lastChar + negativeDiff);
			}
			ret = ret.substring(1, ret.length() - 1) + lastChar;

		} else  {
		    if (num.startsWith("+")) {
		        ret = ret.substring(1);
		    }

		    if (lastChar == '0') {
		        lastChar = positive0EbcdicZoned;
		    } else {
		        lastChar = (char) (lastChar + positiveDiff);
		    }
			ret = ret.substring(0, ret.length() - 1) + lastChar;
		}
		//System.out.print(" - " + lastChar);

		//System.out.println(" -> " + ret);
		return ret;
	}


	/**
	 * This procedure copies a long field into the record
	 *
	 * @param record Full record which is to be updated
	 * @param pos position of the field to recieve the value
	 * @param len length of the field
	 * @param val   new value
	 * @param isPositive wether the field represents only positive integers
	 *
	 */
	public static void setLong(final byte[] record, int pos, int len, long val, boolean isPositive) {
		int i;
		long b;

		checkLength(val, len, isPositive);

		for (i = pos + len - 1; i >= pos; i--) {
			b = val & 255;
			if (b > 127) {
				b = b - 256;
			}
			record[i] = long2byte(b);

			val = val >> 8;
		}
	}

	public static void setBigInt(final byte[] record, int pos, int len, BigInteger val, boolean isPositive) {
		byte[] bytes = val.toByteArray();
		int i;
		byte sb = BYTE_NO_BIT_SET;

		if (bytes.length <= len) {
		} else if (isPositive && bytes.length == len + 1 && (bytes[0] == 0)){
			byte[] tmp = new byte[len];
			System.arraycopy(bytes, 1, tmp, 0, len);
			bytes = tmp;
		} else {
//			System.out.println(" To Big " +  isPositive
//					+ " > " + pos + " " + bytes.length  + " ~ " + len
//					+ " " + bytes[0]);
			throw new RecordException(
					VALUE_IS_TO_BIG_FOR_FIELD,
					new Object[] {isPositive, pos, bytes.length , len, bytes[0]});
		}

		if (val.signum() < 0) {
			sb = BYTE_ALL_BITS_SET;
		}

		for (i = pos ; i < pos + len - bytes.length + 1; i++) {
			record[i] = sb;
		}

		System.arraycopy(bytes, 0, record, pos + len - bytes.length, bytes.length);
	}


	public static void setBigIntLE(final byte[] record, int pos, int len, BigInteger val, boolean isPositive) {
		byte[] bytes = val.toByteArray();
		int i;

		if (bytes.length <= len) {
			int base = pos + bytes.length - 1;
			byte fill = BYTE_NO_BIT_SET;
			if (val.signum() < 0) {
				fill = BYTE_ALL_BITS_SET;
			}
			Arrays.fill(record, base, pos + len, fill);

			for (i = 0; i < bytes.length; i++) {
				record[base - i] = bytes[i];
			}
		} else if (isPositive && bytes.length == len + 1 && (bytes[0] == 0)){
			int base = pos + len - 1;
			for (i = 0; i < len; i++) {
				record[base - i] = bytes[i+1];
			}
		} else {
			throw new RecordException(
					VALUE_IS_TO_BIG_FOR_FIELD,
					new Object[] {isPositive, pos, bytes.length, len,bytes[0]});
		}


	}


	/**
	 * Load a long into the Record (Little Endian format -> low to high
	 * byte).
	 *
	 * @param record Full record which is to be updated
	 * @param pos position in the Record to put the value
	 * @param len field length
	 * @param val value to move to the record
	 * @param isPositive wethear the field represents only positive integers
	 *
	 */
	public static void setLongLow2High(byte[] record,
	        						   int pos, int len,
	        						   long val,
	        						   boolean isPositive) {
		byte[] bytes = BigInteger.valueOf(val).toByteArray();
		int i;

		if (bytes.length <= len) {
			int base = pos + bytes.length - 1;
			byte fill = BYTE_NO_BIT_SET;
			if (val < 0) {
				fill = BYTE_ALL_BITS_SET;
			}
			Arrays.fill(record, base, pos + len, fill);

			for (i = 0; i < bytes.length; i++) {
				record[base - i] = bytes[i];
			}
		} else {
			int base = pos + len - 1;

			checkLength(val, len, isPositive);
			for (i = 0; i < len; i++) {
				record[base - i] = bytes[i];
			}
		}
	}


	/**
	 * Ensure the long value is not to long for the field
	 *
	 * @param val value to check
	 * @param length length of the destination field
	 *
	 */
	public static void checkLength(long val, int length, boolean isPositive) {
		long t = val;
		int i;
		for (i = 1; i < length; i++) {
		    t = t >> 8;
		}
		if (isPositive) {
		    t = t >> 8;
		} else {
		    t = t >> 7;
		}

		if (t > 0) {
			throw new RecordException("Field is to big");
		}

	}

    /**
     * Trims a number string of leading space's and zeros
     *
     * @param s string to be trimmed
     * @return trimmed string
     */
    public static final String numTrim(String s) {
    	return numTrim(s, decimalChar);
    }

    /**
     * Trims a number string of leading space's and zeros
     *
     * @param s string to be trimmed
     * @param decimalCharacter decimal char to use
     * @return trimmed string
     */
   public static final String numTrim(String s, char decimalCharacter) {

        int i = 0;
        int len;
        int ch;
        String pref = "";

        s = s.trim();
        if (s.startsWith("-")) {
            pref = "-";
            s = s.substring(1);
        } else if (s.startsWith("+")) {
            s = s.substring(1);
        }

        if (s.length() == 0) {
        	return s;
        }

        ch = s.charAt(0);
        len = s.length() - 1;
        while ((i < len) && ((ch == ' ') || (ch == '0'))) {
            i += 1;
            ch = s.charAt(i);
        }


        if (i > 0) {
        	//System.out.println(" @@@ " + i + " " + s.charAt(i) + " " + decimalCharacter + " !" + s);
            if (s.charAt(i) == decimalCharacter) {
                i -= 1;
            }
            s = s.substring(i);
        }

        if (decimalCharacter != ',' && (s.indexOf(",") > 0)) {
        	StringBuffer b = new StringBuffer(s.length());
        	int e = s.length();
        	char chr;
        	for (i = 0; i < e; i++) {
        		chr = s.charAt(i);
        		if (chr != ',') {
        			b.append(chr);
        		}
         	}
       		s = b.toString();
        }

        return pref + s.trim();
    }

    /**
     * Checks if a string is a valid integer
     *
     * @param s possibleInteger
     * @return whether it is a integer
     */
    public static final boolean isInt(String s) {
        boolean ret = false;
            String ss = s.trim();
        int len = ss.length();
        int firstIndex = 0;

        if (len > 0) {
            char first = ss.charAt(firstIndex);
            if (first  == '+' || first == '-') {
                if (len == 1) return false;
                firstIndex++;
            }
            for (int i = firstIndex; i < len; i++) {
                if (!Character.isDigit(ss.charAt(i)))
                    return false;
            }
            ret = true;
        }
        return ret;
    }

    /**
     * This method extracts a Copybookname from a file name
     *
     * @param fileName file name
     * @return Copybook name
     */
    public static final String getCopyBookId(final String fileName) {
        String lCopyBook = fileName;
        int pos = lCopyBook.lastIndexOf(Constants.FILE_SEPERATOR);

        if ("\\".equals(Constants.FILE_SEPERATOR) || "/".equals(Constants.FILE_SEPERATOR)) {
        	pos = Math.max(lCopyBook.lastIndexOf("/"), lCopyBook.lastIndexOf("\\"));
        }
        if (pos < 0) {
            pos = lCopyBook.lastIndexOf("/");
        }

        //System.out.println(">> " + pos + " ->" + Constants.FILE_SEPERATOR
    	//	+ "<- " + lCopyBook + " > " + lCopyBook.lastIndexOf("."));
        if (pos >= 0) {
            lCopyBook = lCopyBook.substring(pos + 1);
        }

        pos = lCopyBook.lastIndexOf(".");
        if (pos >= 0) {
            lCopyBook = lCopyBook.substring(0, pos);
        }

        return lCopyBook;
   }

    /**
     * Replaces on string with another in a String bugffer
     *
     * @param in String  to be updated
     * @param from search string
     * @param to replacement string
     */
    public static final StringBuilder replace(String in, String from, String to) {
    	return replace(new StringBuilder(in), from, to);
    }

    /**
     * Replaces on string with another in a String bugffer
     *
     * @param in String buffer to be updated
     * @param from search string
     * @param to replacement string
     */
    public static final StringBuilder replace(StringBuilder in, String from, String to) {
        int start;
        int fromLen = from.length();

        start = in.indexOf(from, 0);
        while (start >= 0) {
            in.replace(start, start + fromLen, to);
            start = in.indexOf(from, start + to.length());
        }

        return in;
    }


    public static byte[] getCsvDelimBytes(String s, String font) {
    	byte[] ret = null;

    	if (s != null) {
			if (s.startsWith("x'")) {
				ret = new byte[] {Conversion.getByteFromHexString(s)};
			} else {
				ret = Conversion.getBytes(s, font);
			}
    	}
		return ret;
    }

    public static byte getByteFromHexString(String s) {
		int b = Integer.parseInt(s.substring(2, 4), 16);
		return long2byte(b);
    }

    public static byte long2byte(long i) {
    	long b = i;
		if (b > 127) {
			b = b - 256;
		}
		return (byte) b;
    }


	public static char getDecimalchar() {
		return decimalChar;
	}


	public static NumberFormat getNumberformat() {
		return numberFormat;
	}

	public static boolean isHtml(String s) {
		//return (s.indexOf('<') >= 0 && s.indexOf("/>") > 0) || (s.indexOf("</") >= 0 && s.indexOf('>') > 0);
		if (s == null) {
			return false;
		}
		String field2check = s.trim().toLowerCase();

		return  field2check.indexOf('<') >= 0
			&&	(	field2check.startsWith("<html>")
				||  field2check.indexOf("<h1>") >= 0
				||  field2check.indexOf("<h0>") >= 0
				||  field2check.indexOf("<h2>") >= 0
				||  field2check.indexOf("<b>") >= 0
				||  field2check.indexOf("<i>") >= 0
				||  field2check.indexOf("<em>") >= 0
				||  field2check.indexOf("<li>") >= 0);
	}

	public static String padFront(String val, int size, char ch) {
		return new StringBuilder()
						.append(getCharArray(size, ch))
						.append(val)
					.toString();
	}
	
	public static final char[] getCharArray(int size, char ch) {
		char[] c = new char[size];
    	Arrays.fill(c, ch);
    	return c;
	}

	public static final String getDefaultSingleByteCharacterset() {
		return defaultSingleByteCharacterset;
	}


	public static final void setDefaultSingleByteCharacterset(
		String defaultSingleByteCharacterset) {

		if (Charset.isSupported(defaultSingleByteCharacterset)
		&& (! (new HoldEbcidicFlag(defaultSingleByteCharacterset)).isMultiByte)) {
			Conversion.defaultSingleByteCharacterset = defaultSingleByteCharacterset;
		}
	}


	public static final boolean isAlwaysUseDefaultSingByteCharset() {
		return alwaysUseDefaultSingByteCharset;
	}


	public static final void setAlwaysUseDefaultSingByteCharset(
			boolean alwaysUseDefaultSingByteCharset) {
		Conversion.alwaysUseDefaultSingByteCharset = alwaysUseDefaultSingByteCharset;
	}


	public static boolean isSingleByteEbcidic(String charset) {
		return getHold(charset).isSingleByteEbcidic;
	}
	
    public static boolean isSingleByte(String fontName) {
    	return ! getHold(fontName).isMultiByte;
    }

    public static boolean isMultiByte(String fontName) {
    	return getHold(fontName).isMultiByte;
    }

 
    private static HoldEbcidicFlag getHold(String charset) {
    	HoldEbcidicFlag hold = holdEbcidicFlag;
		if (charset == null || charset.length() == 0) {
			hold = DEFAULT_CHARSET_DETAILS;
		} else if (! charset.equalsIgnoreCase(hold.charset)) {
			hold = new HoldEbcidicFlag(charset);
			holdEbcidicFlag = hold;
		}
		
		return hold;
    }
    
    public static void setDefaultEbcidicCharacterset(String charset) {
    	if (getHold(charset).isEbcdic) {
    		byte[] b = {(byte) 0xC0, (byte) 0xD0};
    		String s = toString(b, charset);
    		if (s.length() == 2) {
    			positive0EbcdicZoned = s.charAt(0);
    			negative0EbcdicZoned = s.charAt(1);
    		}
    	}
    }
    
    
//    /**
//     * pad string with zero's to format length
//     *
//     * @param dateFormatStr date format string
//     * @param s string to be padded
//     *
//     * @return padded string
//     */
//    public static String padZeros(String dateFormatStr, String s) {
//
//        String ret = s;
//        if (dateFormatStr != null && s.length() < dateFormatStr.length()
//        && (dateFormatStr.startsWith("y") || dateFormatStr.startsWith("M")
//                || dateFormatStr.startsWith("d"))) {
//            ret = "00000000".substring(0, Math.min(8, dateFormatStr.length() - s.length()))
//            + s;
//        }
//        return ret;
//    }
	
    /**
     * Class to hold character-set details
     * 
     * @author Bruce Martin
     *
     */
	public static final class HoldEbcidicFlag {
		public final String charset;
		public final boolean isSingleByteEbcidic, isMultiByte, isEbcdic;
		
		public HoldEbcidicFlag(String charset) {
			super();
			
			if (charset == null || charset.length() == 0) {
				this.charset =  Charset.defaultCharset().name();
			} else {
				this.charset = charset;
			}
			this.isMultiByte = isMultiByteI(charset);
			
			
			byte[] b = getBytes("0", charset);
				
			isEbcdic = b != null && b.length == 1 && b[0] ==  (byte) 0x0F0 ;
			
			
			this.isSingleByteEbcidic = isEbcdic && ! isMultiByte;
		}
		
	    private static boolean isMultiByteI(String fontName) {
			float f = 2;

			if (fontName == null || fontName.length() == 0) {
				f = Charset.defaultCharset().newEncoder().maxBytesPerChar();
			} else if (Charset.isSupported(fontName)) {
				f = Charset.forName(fontName).newEncoder().maxBytesPerChar();
			}

			return (f > 1.0f);
	    }

	}

}
