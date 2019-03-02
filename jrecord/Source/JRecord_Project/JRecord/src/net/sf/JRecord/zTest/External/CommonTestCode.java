package net.sf.JRecord.zTest.External;

import junit.framework.TestCase;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Types.Type;


public class CommonTestCode {

	private static String[] types = new String[200]; 
	static {
		for (int i = 0; i < types.length; i++) {
			types[i] = Integer.toString(i);
		}
		types[Type.ftChar]            = "Type.ftChar";
		types[Type.ftPackedDecimal]   = "Type.ftPackedDecimal";
		types[Type.ftZonedNumeric]    = "Type.ftZonedNumeric";
//		types[Type.ftZonedLeading]    = "Type.ftZonedLeading";
		types[Type.ftBinaryBigEndian] = "Type.ftBinaryBigEndian";
	}
	public static void compare(String id, ExternalField[] expected, ExternalField[] actual) {
		if (expected == null) {
			System.out.println("\t}, {");
			for (int i = 0; i < actual.length; i++) {
				System.out.println("\t\tCommonTestCode.newFldDtl(\"" + actual[i].getName() 
						+ "\", " + types[actual[i].getType()]
						+ ", "   + actual[i].getPos()
						+ ", "   + actual[i].getLen()
						+ ", "   + actual[i].getDecimal()
						+ "),");
			}
		} else {
			TestCase.assertEquals(id, expected.length, actual.length);
			for (int i = 0; i < actual.length; i++) {
				String s = id + ", " + i + ") " + expected[i].getName() + " " + types[expected[i].getType()]
						+ " " + expected[i].getPos() + ", " + expected[i].getLen();
				TestCase.assertEquals(s, expected[i].getName(), actual[i].getName());
				TestCase.assertEquals(s, expected[i].getPos(),  actual[i].getPos());
				TestCase.assertEquals(s, expected[i].getLen(),  actual[i].getLen());
				TestCase.assertEquals(s, expected[i].getType(), actual[i].getType());
				TestCase.assertEquals(s, expected[i].getDecimal(), actual[i].getDecimal());
			}
		}
	}

	public static ExternalField newFldDtl(String name, int type, int pos, int len, int decimal) {
		return new ExternalField(pos, len, name, "", type, decimal,
				0, "", "", "", 0);
	}
}
