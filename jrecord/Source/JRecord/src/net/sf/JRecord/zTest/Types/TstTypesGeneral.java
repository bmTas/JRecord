/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.zTest.Types;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Random;

import junit.framework.TestCase;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.Types.TypeSignSeparate;
import net.sf.JRecord.zTest.Common.TestCommonCode;


/**
 * General Type Tests
 * 
 * @author Bruce Martin
 *
 */
public class TstTypesGeneral extends TestCase {

	public static final int[] NUMERIC_TYPES = {

	 	Type.ftNumLeftJustified,
	 	Type.ftNumRightJustified,
	 	Type.ftNumRightJustifiedPN,
	 	Type.ftNumRightJustCommaDp,
	 	Type.ftNumRightJustCommaDpPN,
	 	Type.ftNumZeroPadded,
	 	Type.ftNumZeroPaddedPN,
	 	Type.ftNumZeroPaddedPositive,
	 	Type.ftNumCommaDecimal,
	 	Type.ftNumCommaDecimalPN,
	 	Type.ftNumCommaDecimalPositive,

	 	Type.ftAssumedDecimal,
	 	Type.ftAssumedDecimalPositive,
	 	Type.ftSignSeparateLead,
	 	Type.ftSignSeparateTrail,
	 	Type.ftSignSepLeadActualDecimal,
	 	Type.ftSignSepTrailActualDecimal,
	 	Type.ftDecimal,
	 	Type.ftBinaryInt,
	 	Type.ftPostiveBinaryInt,
	 	Type.ftBinaryIntPositive,
	 	Type.ftFloat,
	 	Type.ftDouble,
	 	Type.ftNumAnyDecimal,
	 	Type.ftNumOrEmpty,
	 	Type.ftPositiveNumAnyDecimal,
	 	Type.ftBit,

	 	Type.ftPackedDecimal,
	 	Type.ftPackedDecimalPostive,
	 	Type.ftZonedNumeric,
	 	Type.ftBinaryBigEndian,
	 	Type.ftBinaryBigEndianPositive,
	 	Type.ftPositiveBinaryBigEndian,
	 	Type.ftRmComp,
	 	Type.ftRmCompPositive,

	 	Type.ftFjZonedNumeric,
	 	Type.ftGnuCblZonedNumeric,
	};

	public static final int[] BINARY_TYPES = {
		Type.ftHex,
		Type.ftDecimal,
		Type.ftBinaryInt,
		Type.ftPostiveBinaryInt,
	 	Type.ftBinaryIntPositive,

		Type.ftFloat,
		Type.ftDouble,

		Type.ftBit,

	 	Type.ftPackedDecimal,
	 	Type.ftPackedDecimalPostive,
	 	Type.ftBinaryBigEndian,
	 	Type.ftBinaryBigEndianPositive,
	 	Type.ftPositiveBinaryBigEndian,
	 	Type.ftRmComp,
	 	Type.ftRmCompPositive,

		Type.ftCharRestOfRecord
	};

	public static final int[] POSITIVE_TYPES = {
		Type.ftPostiveBinaryInt,
	 	Type.ftBinaryIntPositive,

		Type.ftAssumedDecimalPositive,
	 	Type.ftPackedDecimalPostive,
	 	Type.ftPositiveNumAnyDecimal,
	 	Type.ftBinaryBigEndianPositive,
	 	Type.ftPositiveBinaryBigEndian,
	 	Type.ftRmCompPositive,
	};

	private TypeManager typeManager = new TypeManager();
	private byte[] bytes = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};


	public void testNumeric() {
		TypeManager m = TypeManager.getInstance();
		HashSet<Integer> numTypes = new HashSet<Integer>(NUMERIC_TYPES.length + 25);
		HashSet<Integer> binTypes = new HashSet<Integer>(BINARY_TYPES.length + 25);

		for (int i = 0; i < 200; i++) {
			if (m.getType(i).isBinary()) {
				System.out.print("  " + i);
			}
		}

		for (int typeId : NUMERIC_TYPES) {
			assertTrue("Numeric Type: " + typeId, m.getType(typeId).isNumeric());
			numTypes.add(typeId);
		}

		for (int i = 0; i < 200; i++) {
			if (! numTypes.contains(i)) {
				assertFalse("Char Type: " + i , m.getType(i).isNumeric());
			}
		}

		byte[] b;
		String s= "";
		for (int i = 0; i < 200; i++) {
			FieldDetail fld = getType(1, 4, i, Conversion.DEFAULT_ASCII_CHARSET);
			try {
				b = typeManager.getType(i).setField(bytes, fld.getPos(), fld, CommonBits.NULL_VALUE);
				if (i == Type.ftFloat || i == Type.ftDouble) {
					assertEquals("Assign NULL to number: " + i, "0.0", typeManager.getType(i).getField(b, fld.getPos(), fld).toString());
//				} else 	if (i == Type.ftNumAnyDecimal   || i == Type.ftPositiveNumAnyDecimal) {
//					assertEquals("Assign NULL to number: " + i, "0", typeManager.getType(i).getField(b, fld.getPos(), fld).toString());
				} else if (TypeManager.isNumeric(i)) {
					assertEquals("Assign NULL to number: " + i, "0", typeManager.getType(i).getField(b, fld.getPos(), fld).toString());
				} else if (i == Type.ftHex) {
					assertEquals("Assign NULL to number: " + i, "00000000", typeManager.getType(i).getField(b, fld.getPos(), fld).toString());
				} else if (i == Type.ftCharNoTrim) {
					assertEquals("Assign NULL to char: " + i, "    ", typeManager.getType(i).getField(b, fld.getPos(), fld).toString());
				} else {
					assertEquals("Assign NULL to char: " + i, "", typeManager.getType(i).getField(b, fld.getPos(), fld).toString());
				}
			} catch (RecordException e) {
				s = s + "\nError Type: " + i + " , " + e;
				e.printStackTrace();
			}
		}
		System.out.println(s);
		assertEquals("", s);


		for (int typeId : BINARY_TYPES) {
			assertTrue("Bin Type: " + typeId, m.getType(typeId).isBinary());
			binTypes.add(typeId);
		}

		for (int i = 0; i < 200; i++) {
			if (! binTypes.contains(i)) {
				assertFalse("Type: " + i, m.getType(i).isBinary());
			}
		}
	}

	/**
	 * Check Sign Separate Type
	 * @throws RecordException
	 */
	public void testSignSeperate() throws RecordException {
		String[][] tstData = {
				{"1", "+001", "001+"},
				{"34", "+034", "034+"},
				{"432", "+432", "432+"},
				{"+1", "+001", "001+"},
				{"+34", "+034", "034+"},
				{"+432", "+432", "432+"},
				{"-1", "-001", "001-"},
				{"-34", "-034", "034-"},
				{"-432", "-432", "432-"},
		};
		byte[] b = {32, 32, 32, 32, 32, 32};
		TypeSignSeparate leadingSignType = new TypeSignSeparate(Type.ftSignSeparateLead);
		FieldDetail leadingSignField = getType(1, 4, Type.ftSignSeparateLead, "");
		TypeSignSeparate trailingSignType = new TypeSignSeparate(Type.ftSignSeparateTrail);
		FieldDetail trailingSignField = getType(1, 4, Type.ftSignSeparateTrail, "");
		
		
		for (String[] tstVal: tstData) {
			assertEquals(tstVal[1], leadingSignType.formatValueForRecord(leadingSignField, tstVal[0]));
			assertEquals(tstVal[1] + "  ", new String(leadingSignType.setField(b, 1, leadingSignField, tstVal[0])));
			assertEquals(tstVal[2], trailingSignType.formatValueForRecord(trailingSignField, tstVal[0]));
			assertEquals(tstVal[2] + "  ", new String(trailingSignType.setField(b, 1, trailingSignField, tstVal[0])));
		}
		
	}
	
	public void testAssignment() throws RecordException,  Exception {
		String[] charsets1 = {Conversion.DEFAULT_ASCII_CHARSET, "cp1525", "utf-8", "CP037", "IBM273"};
		String[] charsets2 = {"utf-16", "utf-16be"};
		    	String cobolCopybook
    		= "      01 COMPANY-RECORD.\n"
    		+ "         05 COMPANY-NAME     PIC X(30).\n";
    	
    	for (String c : charsets1) {
    		LayoutDetail schema = TestCommonCode.getLayoutFromCobolStr(
    				cobolCopybook, "COMPANY-RECORD",
    				CopybookLoader.SPLIT_NONE, c, ICopybookDialects.FMT_FUJITSU);
    		LayoutDetail csvSchema = TestCommonCode.getCsvLayout(Constants.IO_BIN_TEXT, c, ";", "", false, 0);
    		
	    	System.out.print("\t" + c);
	    	tstAssignment(new Line(schema), c, true, 4);
	  		
	    	tstAssignment(new CharLine(schema, ""), c, false, 4);
	    	
	    	tstAssignment(new Line(csvSchema), c, false, -121);
	  		
	    	tstAssignment(new CharLine(csvSchema, ""), c, false, -121);

    	}

       	for (String c : charsets2) {
    		LayoutDetail schema = TestCommonCode.getLayoutFromCobolStr(
    				cobolCopybook, "COMPANY-RECORD",
    				CopybookLoader.SPLIT_NONE, c, ICopybookDialects.FMT_FUJITSU);
       		LayoutDetail csvSchema = TestCommonCode.getCsvLayout(Constants.IO_BIN_TEXT, c, ";", "", false, 0);
	    	System.out.print("\t" + c);
	  		
	    	tstAssignment(new CharLine(schema, ""), c, false, 4);
	    	tstAssignment(new CharLine(csvSchema, ""), c, false, -121);
    	}
	}

	
	private void tstAssignment(AbstractLine l, String charset, boolean tstBinaryTypes, int size) throws RecordException {
		String[] values1 = {"0", "5", "10", "32", "432", "6543", "-5", "-10", "-32", "-432"};
		String[] values2 = {"1.1", "-2.1", "54.3"};
		TypeManager m = TypeManager.getInstance();
		for (int i = 0; i<200; i++) {
			boolean skipForChar = false;
			switch (i) {
			case Type.ftFloat:
			case Type.ftDouble:
			case Type.ftHex:
			case Type.ftBit:
				break;
			case Type.ftCharRestOfRecord:
			case Type.ftCharNullPadded:
			case Type.ftCharNullTerminated:
				skipForChar = true;
			
			default:
		
				if (tstBinaryTypes 
				|| ! (  skipForChar || m.getType(i).isBinary())) {				
					tstSet(values1, charset, i, l, getType(1, size, i, charset), skipForChar);
					tstSet(values2, charset, i, l, getType(1, size, 1, i, charset), skipForChar);
				}
			}
		}
	}
	
	@SuppressWarnings("deprecation")
	private void tstSet(String[] values, String charset, int typeId, AbstractLine l, FieldDetail fld, boolean skipForChar) throws RecordException {
		Type t = TypeManager.getInstance().getType(typeId);
		
		for (String v : values) {
			if ((t instanceof TypeNum && ((TypeNum) t).isPositive() && v.startsWith("-"))
			|| (   v.length() == 4 
			   && (t instanceof TypeSignSeparate && (! v.startsWith("-")))
			     || typeId == Type.ftRmComp || typeId == Type.ftRmCompPositive)) {
				
			} else {
				if (typeId == Type.ftNumAnyDecimal && fld.getDecimal() == 0 && "0".equals(v)) {
					System.out.print( "* >" + v + "<");
				}
				l.setField(fld, v);
				
				//System.out.print("\t" + i);
				AbstractFieldValue fieldValue = l.getFieldValue(fld);
				String actualValue = fieldValue.asString();
				switch (typeId) {
				case Type.ftCharRightJust:
					if (fld.isFixedFormat()) {
						assertEquals(charset + " Test number: " + typeId, "    ".substring(v.length()) + v, actualValue);
						break;
					}
				case Type.ftCharNoTrim:
//					if (v.length() < 4 && v.equals(actualValue)) {
//						System.out.print("* " + fld.isFixedFormat());
//						if (fld.isFixedFormat()) {
//							actualValue = fieldValue.asString();
//						}
//					} else {
					if (fld.isFixedFormat()) {
						assertEquals(charset + " Test number: " + typeId, v + "    ".substring(v.length()), actualValue);
					}
					
					break;
				default:
					assertEquals(charset + " Test number: " + typeId, v, actualValue);
				}
				
				if (t.isBinary() || skipForChar 
//				|| typeId == Type.ftNumAnyDecimal || typeId == Type.ftPositiveNumAnyDecimal
				|| (! fld.isFixedFormat())) {
					
				} else {
					String id = charset + " Test number: " + typeId + " " + fld.getDecimal() + ">" + v + "< " + (l instanceof CharLine);
					if (typeId == Type.ftNumAnyDecimal && fld.getDecimal() == 0 && "0".equals(v)) {
						System.out.print(("\t .>" + fld.getDecimal()) + " " + (l instanceof CharLine)
								+ ")" + Conversion.toString(l.getData(1, fld.getLen()), charset) + "(");
					}
					
					String recValue = t.formatValueForRecord(fld, v);
					if ((t.isNumeric() || typeId == Type.ftCharRightJust)
					) {
						assertEquals(id, 
							l.getFullLine().substring(0, fld.getLen()),
							"    ".substring(recValue.length()) + recValue);
					} else {
						assertEquals(id, 
								l.getFullLine().substring(0, fld.getLen()),
								recValue + "    ".substring(recValue.length()));
					}
				}
			}
		}
	}


	/**
	 * Tests various Character-sets / line type combinations for Fixed-length-lines
	 * @throws Exception
	 */
	public void testFixedLineSet() throws RecordException {
		String initialValue = "123456789.123456789";
		String[] charsets1 = {Conversion.DEFAULT_ASCII_CHARSET, "cp1525", "CP037", "IBM273", "utf-8", "utf-16be"};
    	String cobolCopybook
    		= "      01 COMPANY-RECORD.\n"
    		+ "         05 COMPANY-NAME     PIC X(30).\n";
		AbstractLine[] lines = new AbstractLine[charsets1.length * 2 - 2];
		int k = 0;
    	for (int i = 0; i < charsets1.length; i++) {
    		String c = charsets1[i];
    	
    		LayoutDetail schema = TestCommonCode.getLayoutFromCobolStr(
    				cobolCopybook, "COMPANY-RECORD",
    				CopybookLoader.SPLIT_NONE, c, ICopybookDialects.FMT_FUJITSU);
    		if (i < charsets1.length - 2) {
    			lines[k] = new Line(schema);
    			lines[k + 1] = new CharLine(schema, initialValue);
    			lines[k].setData(initialValue);
    			k += 2;
    		} else {
    			lines[k++] = new CharLine(schema, initialValue);
    		}
    	}
    	tstLineAssignment(lines, true);
	}
	
	/**
	 * Tests various Character-sets / line type combinations for Csv-lines
	 * @throws Exception
	 */
	public void testCsvLineSet() throws Exception {
		String initialValue = "123456789.123456789";
		String[] charsets1 = {"", "cp1525", "CP037", "IBM273", "utf-8", "utf-16be"};
		AbstractLine[] lines = new AbstractLine[charsets1.length * 2 - 2];
		int k = 0;
		
    	for (int i = 0; i < charsets1.length; i++) {
    		String c = charsets1[i];
    		LayoutDetail schema = TestCommonCode.getCsvLayout(Constants.IO_BIN_TEXT, c, ";", "", false, 0);
   
    		if (i < charsets1.length - 2) {
    			lines[k] = new Line(schema);
    			lines[k + 1] = new CharLine(schema, initialValue);
    			lines[k].setData(initialValue);
    			k += 2;
    		} else {
    			lines[k++] = new CharLine(schema, initialValue);
    		}
    	}
    	tstLineAssignment(lines, false);
	}

	/**
	 * 
	 * @param l
	 * @param fixed
	 * @throws RecordException
	 */
	private void tstLineAssignment(AbstractLine[] l, boolean fixed) throws RecordException {
		String[] values1 = {"0", "5", "10", "32", "432", "6543", "-5", "-10", "-32", "-432"};
		String[] values2 = {"1.1", "-2.1", "54.3"};
		TypeManager m = TypeManager.getInstance();
		for (int i = 0; i<200; i++) {
			switch (i) {
			case Type.ftFloat:
			case Type.ftDouble:
			case Type.ftHex:
			case Type.ftBit:
				break;
			case Type.ftCharRestOfRecord:
			case Type.ftCharNullPadded:
			case Type.ftCharNullTerminated:
				break;
			default:
		
				if ( ! (  m.getType(i).isBinary())) {			
					for (int j = 0; j < values1.length; j++) {
						String v1 = values1[j];
						String v2 = values2[j % values2.length];
						setLine(l[0], i, v1, v2, fixed);
						for (int k = 1; k < l.length; k++) {
							String fontName = l[k].getLayout().getFontName();
							if ("CP037".equals(fontName) && i == 32 && "5".equals(v1) && "-2.1".equals(v2)) {
								System.out.print("+");
							}
							if ("IBM273".equals(fontName)
							&& (i == Type.ftZonedNumeric)) {
								
							} else {
								String id = fontName
										+ " Type: " + i
										+ " values: " + v1 + ", " + v2
										+ " " + (l[k] instanceof CharLine);
								setLine(l[k], i, v1, v2, fixed);
								assertEquals(id, l[0].getFullLine(), l[k].getFullLine());
							}
						}
					}
				}
			}
		}
	}

	/**
	 * Set 2 fields in a line
	 * @param line line to be updated
	 * @param type type to test
	 * @param v1 value to assign to the first field 
	 * @param v2 value to assign to the second field 
	 * @param fixed wether it is a fixed-length line (or a CSV line)
	 * @throws RecordException
	 */
	private void setLine(AbstractLine line, int type, String v1, String v2, boolean fixed) throws RecordException {
		LayoutDetail ld = line.getLayout();
		String charset = ld.getFontName();
		Type t = TypeManager.getInstance().getType(type);
		boolean positive = (t instanceof TypeNum) && ((TypeNum) t).isPositive();
		FieldDetail fld1, fld2;
		if (fixed) { // Create fixed width fields
			fld1 = getType(1, 4, 0, type, charset);
			fld2 = getType(5, 4, 1, type, charset);
		} else { // Create CSV fields
			fld1 = getType(1, -1, 0, type, charset);
			fld2 = getType(2, -1, 1, type, charset);
		}
		
		if (positive) {
			if (v1.startsWith("-")) {
				v1 = Integer.toString(type);
			}
			if (v2.startsWith("-")) {
				v2 = Integer.toString(type);
			}
		}
		
		if ( v1.length() < 4 
		|| (type != Type.ftSignSeparateLead && type != Type.ftSignSeparateTrail) 
		|| (! fixed)
		|| v1.startsWith("-")) {
			line.getFieldValue(fld1).set(v1);
		}
		line.getFieldValue(fld2).set(v2);
	}

	/**
	 * Check Assignment / retrieval for various numeric types
	 * @throws RecordException
	 */
	public  void testNumbers() throws RecordException {
		Random r = new Random();
		FieldDetail fd;
		int val;
		String res = "";
		boolean ok = true;

		for (int type : NUMERIC_TYPES) {
			switch (type) {
			case Type.ftBit: break;
			default:
				fd = getType(1, 8, type, Conversion.DEFAULT_ASCII_CHARSET);
				for (int i = 0; i < 5000; i++) {
					val = r.nextInt(10000);

					try {
						setFldValue(fd, val);
						res = getFldValue(fd);
						assertEquals("Type: " + type, val, (new BigDecimal(res.trim())).intValue());
					} catch (Exception e) {
						System.out.println();
						System.out.println();
						System.out.println("Type: " + type + " Count: " + i + " Val: " + val + " result: >" + res + "<");
						System.out.println();
						System.out.println();
						e.printStackTrace();
						ok = false;
						break;
					}
				}
			}
		}

		assertTrue("Check abends ", ok);
	}

	/**
	 * Check positive Types are indeed defined as positive
	 */
	public void testPositive() {
		TypeManager m = TypeManager.getInstance();

		for (int typeId : POSITIVE_TYPES) {
			assertTrue("Positive Type: " + typeId, m.getType(typeId).isNumeric());
		}

	}


	  /**
     * Get field value
     * @param fld field definition
     * @return value of the field
     */
    private String getFldValue(FieldDetail fld) {
        return typeManager.getType(fld.getType()).getField(bytes, fld.getPos(), fld).toString();
    }

    /**
     * Set Fields value
     * @param fld field definition
     * @param val value to assign
     * @throws RecordException any error that occurs
     */
    private void setFldValue(FieldDetail fld, Integer val) throws RecordException {
        typeManager.getType(fld.getType()).setField(bytes, fld.getPos(), fld, val);
    }

	private FieldDetail getType(int pos, int len, int type, String charset) {
		return getType(pos, len, 0, type, charset);
		
    }

	/**
	 * Create field
	 * 
	 * @param pos field position
	 * @param len field length
	 * @param decimal number of decimal places
	 * @param type type of the field
	 * @param charset character set.
	 * 
	 * @return the requested field
	 */
	private FieldDetail getType(int pos, int len, int decimal, int type, String charset) {


		FieldDetail field = new FieldDetail("", "", type, decimal, charset, -1, "");

		if (len > 0) {
			field.setPosLen(pos, len);
		} else {
			field.setPosOnly(pos);
		}

		return field;
	}

}
