package net.sf.JRecord.zTest.Types;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;

/**
 * Basic Tests on big endian and little endian numbers
 * 
 * @author Bruce Martin
 *
 *
 */
public class TstTypesSetValues extends TestCase {
	private static final BigDecimal bdDecimal = new BigDecimal("0.123");

	private byte[] bytes = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

	private static String[][] numValues = {
		{"1", "1", "1.0", "1.00", "1.000", "1.0000",},
		{"10",   "10", "10.0", "10.00", "10.000", "10.0000",},
		{"10.1", "10", "10.1", "10.10", "10.100", "10.1000",},
		{"10.1234", "10", "10.1", "10.12", "10.123", "10.1234",},
		{"1.234567e2", "123", "123.4", "123.45", "123.456", "123.4567",},
	};
	
	public void testBigEndian1() throws RecordException {
		tstField1(Type.ftBinaryBigEndian);
		tstField2(Type.ftBinaryBigEndian);
	}	
	
	public void testLittleEndian1() throws RecordException {
		tstField1(Type.ftBinaryInt);
		tstField2(Type.ftBinaryInt);
	}
	
	
	public void testPackedDecimal1() throws RecordException {
		tstField1(Type.ftPackedDecimal);
		tstField2(Type.ftPackedDecimal);
		tstField3(Type.ftPackedDecimal);
		tstField1(Type.ftDecimal);
		tstField2(Type.ftDecimal);
	}
	
	
	public void testZonedDecimal1() throws RecordException {
		tstField1(Type.ftZonedNumeric);
		tstField2(Type.ftZonedNumeric);
		tstField1(Type.ftFjZonedNumeric);
		tstField2(Type.ftFjZonedNumeric);
	}

//	String[] charsets = TstConstants.EBCDIC_CHARSETS;

		String[] typeNames = {
				"ftNumLeftJustified", "ftNumRightJustified", "ftNumZeroPadded ",
				"ftAssumedDecimal",   "ftSignSeparateLead",  "ftSignSeparateTrail",
				"ftNumZeroPaddedPN"};
		int[] typesToTest = {
				Type.ftNumLeftJustified,  
				Type.ftNumRightJustified,
				Type.ftNumZeroPadded,     
				Type.ftAssumedDecimal,    
				Type.ftSignSeparateLead,  
				Type.ftSignSeparateTrail, 
				Type.ftNumZeroPaddedPN,
		};	
	public void testGeneral1() throws RecordException {
		
		for (int i = 0; i < typesToTest.length; i++) {
			int typeId = typesToTest[i];
			System.out.println("Testing: " + i + " ~ "+ typeId + " " + typeNames[i]);
			tstField1(typeId);
			tstField2(typeId);
		}

	}
	
	

	public void testGeneral2() throws RecordException {
		
		for (String charset : TstConstants.EBCDIC_SINGLE_BYTE_CHARSETS) {
			System.out.println();
			System.out.print("Testing: " + charset + " -");
			for (int i = 0; i < typesToTest.length; i++) {	
				int typeId = typesToTest[i];
				System.out.print(" " + i + " ~ "+ typeId + " " + typeNames[i]);
				tstField1(typeId, 0, charset, 61);
				tstField2(typeId, 0, charset, 67);
			}
		}
	}
	
	public void testGeneral3() throws RecordException {
		boolean[] toTest = new boolean[200];
		Arrays.fill(toTest, true);
		for (int i = 0; i < typesToTest.length; i++) {
			toTest[i] = false;
		}
		
		toTest[Type.ftFloat] = false;
		toTest[Type.ftDouble] = false;
		toTest[Type.ftNumAnyDecimal] = false;
		toTest[Type.ftPositiveNumAnyDecimal] = false;
		toTest[Type.ftBit] = false;
	
		for (String charset : TstConstants.EBCDIC_SINGLE_BYTE_CHARSETS) {
			for (int i = 0; i < 200; i++) {
				if (toTest[i] && TypeManager.isNumeric(i)) {
					tstField1(i, 0, charset, 361);
					tstField2(i, 0, charset, 367);
				}
			}
		}
	}



	public void testBigEndian3() throws RecordException {
		tstField3(Type.ftBinaryBigEndian);
	}
	

	public void testLittleEndian3() throws RecordException {
		tstField3(Type.ftBinaryInt);
	}

	
	/**
	 * Checking bytes set correctly for Big endian binary integer
	 * 
	 * @throws RecordException any error that occurs 
	 */
	public void testBigEndianSet4() throws RecordException {
		
		tst4(Type.ftBinaryBigEndian, 0);
	}
	
	/**
	 * Checking bytes set correctly for little endian binary integer
	 * 
	 * @throws RecordException any error that occurs 
	 */
	public void testLittleEndianSet4() throws RecordException {
		
		tst4(Type.ftBinaryInt, 1);
	}
	

	private void tstField1(int typeId) throws RecordException{
		for (int i = 0; i < 5; i++) {
			tstField1(typeId, i, "", 11);
		}
	}
	

	private void tstField1(int typeId, int decimalPlaces, String font, int inc) throws RecordException{
		Type t = (Type) TypeManager.getInstance().getType(typeId);
		FieldDetail fldDef = new FieldDetail("", "", typeId, decimalPlaces, font, 0, "");
		if (t.isBinary()) {
			fldDef.setPosLen(1, 8);
		} else {
			fldDef.setPosLen(1, 13);
		}
		byte[] bb;
		Object v;
		String id = "Testing " + font + " " + typeId + " " + t.getClass().getName() + " : ";
		for (int j = 0; j < numValues.length; j++) {
			bb = t.setField(bytes, 1, fldDef, numValues[j][0]);
					
			v = t.getField(bb, 1, fldDef);
			assertEquals(id + numValues[j][0] + " " + decimalPlaces + ", " + j, numValues[j][decimalPlaces+1], v.toString());
			
			if (t instanceof TypeNum && ! ((TypeNum) t).isPositive()) {
				//TODO: Here
				bb = t.setField(bytes, 1, fldDef, "-" + numValues[j][0]);
				
				v = t.getField(bb, 1, fldDef);
				assertEquals(id + numValues[j][0] + " " + decimalPlaces + ", " + j, "-" +  numValues[j][decimalPlaces+1], v.toString());
			}
		}
		
		String s;
		
		for (int i = 0; i < 1200000; i+=inc) {
			s = Integer.toString(i);
			bb = t.setField(bytes, 1, fldDef, s);
			v = t.getField(bb, 1, fldDef);
			
			if (decimalPlaces == 0) {
				assertEquals(id + s + " " + decimalPlaces, s, v.toString());
			} else {
				assertEquals(id + s + " " + decimalPlaces, s + ".0000".substring(0, decimalPlaces + 1), v.toString());
				
				bb = t.setField(bytes, 1, fldDef, s + ".123");
				v = t.getField(bb, 1, fldDef);
				assertEquals(id + s + " " + decimalPlaces, s + ".1230".substring(0, decimalPlaces + 1), v.toString());
			}
		}
	}
	
	
	private void tstField2(int typeId) throws RecordException{
		for (int i = 0; i < 5; i++) {
			tstField2(typeId, i, "", 7);
		}
	}
	
	/**
	 * Test assigning BigInteger / Big decimal to fields
	 * @param typeId type id to test
	 * @param decimalPlaces number of decimal places
	 * @throws RecordException
	 */
	private void tstField2(int typeId, int decimalPlaces, String font, int inc) throws RecordException{
		Type t = TypeManager.getInstance().getType(typeId);
		FieldDetail fldDef = new FieldDetail("", "", typeId, decimalPlaces, "", 0, "");
		if (t.isBinary()) {
			fldDef.setPosLen(1, 8);
		} else {
			fldDef.setPosLen(1, 13);
		}
		byte[] bb;

		
		String s;
		
		String id = "Testing " + font + " " + typeId + " " + t.getClass().getName() + " : ";

		for (int i = 0; i < 1200000; i+=inc) {
			s = Integer.toString(i);
			bb = t.setField(bytes, 1, fldDef, s);
			
			BigDecimal bd = new BigDecimal(i);
			
			if (decimalPlaces == 0) {
				bb = t.setField(bytes, 1, fldDef, bd);
				assertEquals(id + s + " " + decimalPlaces, s, t.getField(bb, 1, fldDef).toString());
				bb = t.setField(bytes, 1, fldDef, bd.add(bdDecimal));
				assertEquals(id + s + " " + decimalPlaces, s, t.getField(bb, 1, fldDef).toString());
				bb = t.setField(bytes, 1, fldDef, BigInteger.valueOf(i));
				assertEquals(id + s + " " + decimalPlaces, s, t.getField(bb, 1, fldDef).toString());
			} else {			
				bb = t.setField(bytes, 1, fldDef, bd.add(bdDecimal));
				assertEquals(id + s + " " + decimalPlaces, s + ".1230".substring(0, decimalPlaces + 1), t.getField(bb, 1, fldDef).toString());
				bb = t.setField(bytes, 1, fldDef, BigInteger.valueOf(i));
				assertEquals(id + s + " " + decimalPlaces, s + ".0000".substring(0, decimalPlaces + 1), t.getField(bb, 1, fldDef).toString());
			}
		}
	}

	

	private void tstField3(int typeId) throws RecordException{
		for (int i = 0; i < 5; i++) {
			tstField3(typeId, i);
		}
	}
	
	private void tstField3(int typeId, int decimalPlaces) throws RecordException{
		Type t = TypeManager.getInstance().getType(typeId);

		FieldDetail fldDef = new FieldDetail("", "", typeId, decimalPlaces, "", 0, "");
		if (t.isBinary()) {
			fldDef.setPosLen(1, 8);
		} else {
			fldDef.setPosLen(1, 11);
		}
		byte[] bb;
		BigDecimal bd;
		
		String ss, s;
		
		for (int ii = 0; ii < 500000; ii+=1) {
			bd = new BigDecimal(ii).scaleByPowerOfTen(-3);
			ss = bd.toString() + "0";
			s = ss.substring(0, ss.lastIndexOf('.'));
			bb = t.setField(bytes, 1, fldDef, s);
			
			if (decimalPlaces == 0) {
				bb = t.setField(bytes, 1, fldDef, ss);
				assertEquals("Testing " + ss + " " + decimalPlaces, s, t.getField(bb, 1, fldDef).toString());
				bb = t.setField(bytes, 1, fldDef, bd);
				assertEquals("Testing " + ss + " " + decimalPlaces, s, t.getField(bb, 1, fldDef).toString());
//				bb = t.setField(bytes, 1, fldDef, new BigInteger(s));
//				assertEquals("Testing " + ss + " " + idx, s, t.getField(bb, 1, fldDef).toString());
			} else {			
				bb = t.setField(bytes, 1, fldDef, ss);
				assertEquals("Testing " + ss + " " + decimalPlaces, ss.substring(0, decimalPlaces + s.length() + 1), t.getField(bb, 1, fldDef).toString());
				bb = t.setField(bytes, 1, fldDef, bd);
				assertEquals("Testing " + ss + " " + decimalPlaces, ss.substring(0, decimalPlaces + s.length() + 1), t.getField(bb, 1, fldDef).toString());
				bb = t.setField(bytes, 1, fldDef, new BigInteger(s));
				assertEquals("Testing " + ss + " " + decimalPlaces, s + ".0000".substring(0, decimalPlaces + 1), t.getField(bb, 1, fldDef).toString());
			}
		}
	}

	
	
	/**
	 * Testing 2 byte binary number is set correctly
	 * @param t type definition
	 * @param typeId type id
	 * @param adj index adjustment 0 for Big Endian, 1 for little endian
	 * @throws RecordException any error thrown
	 */
	private void tst4(int typeId, int adj) throws RecordException {
		Type t = TypeManager.getInstance().getType(typeId);

		FieldDetail fldDef = new FieldDetail("", "", typeId, 0, "", 0, "");
		fldDef.setPosLen(1, 2);
		byte[] bb;
		
		for (int i = 0; i < 100; i++) {
			bb = t.setField(bytes, 1, fldDef, i);
			assertEquals("Test a: " + i, i, bb[1 - adj]);
			assertEquals("Test a: " + i, 0, bb[adj]);
			
			bb = t.setField(bytes, 1, fldDef, i * 256 + i);
			assertEquals("Test a: " + i, i, bb[1 - adj]);
			assertEquals("Test a: " + i, i, bb[adj]);
			
			
			bb = t.setField(bytes, 1, fldDef, i * 256);
			assertEquals("Test a: " + i, 0, bb[1 - adj]);
			assertEquals("Test a: " + i, i, bb[adj]);
		}
	}

}
