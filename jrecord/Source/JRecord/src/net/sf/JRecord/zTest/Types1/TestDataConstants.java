package net.sf.JRecord.zTest.Types1;

import net.sf.JRecord.Common.FieldDetail;

public class TestDataConstants {
	public static final String TEST_DATA_DIRECTORY = "F:/Work/EclipseWorkspaces/workspace/JRecord/src/net/sf/JRecord/zTest/Types1/";
	


	public static String getTestDataFileName(String charset) {
		String s = charset;
		if (s.length() == 0) {
			s = "Std";
		}
		
		return TEST_DATA_DIRECTORY + "TestData_" + s +".txt";
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
