package net.sf.JRecord.zTest.Types1;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;

public class TestDataConstants {

	public static String getTestDataFileName(String charset) {
		String s = charset;
		if (s.length() == 0 || Conversion.DEFAULT_ASCII_CHARSET.equals(s)) {
			s = "Std";
		}

		return TestDataConstants.class.getResource( "TestData_" + s +".txt").getFile();
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
	public static FieldDetail getType(int pos, int len, int decimal, int type, String charset) {


		FieldDetail field = new FieldDetail("", "", type, decimal, charset, -1, "");

		if (len > 0) {
			field.setPosLen(pos, len);
		} else {
			field.setPosOnly(pos);
		}

		return field;
	}


}
