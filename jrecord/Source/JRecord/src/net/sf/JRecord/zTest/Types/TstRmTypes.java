package net.sf.JRecord.zTest.Types;

import junit.framework.TestCase;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeRmComp;
import net.sf.JRecord.Types.TypeRmCompPositive;


public class TstRmTypes  extends TestCase  {
	
	byte[] record1 = {0,0,1,2,3,4,5,0,0,0};
	byte[] record2 = {0,0,2,3,4,5,6};
	byte[] record3 = {0,0,2,3,4,5};
	byte[] record4 = {0,0,2,3,4,5,0};

	byte[] record11 = {0,0,1,2,3,4,TypeRmComp.POSITIVE,0,0,0};
	byte[] record12 = {0,0,1,2,3,4,TypeRmComp.NEGATIVE,0,0,0};
	byte[] record21 = {0,0,2,3,4,5,TypeRmComp.POSITIVE};
	byte[] record22 = {0,0,2,3,4,5,TypeRmComp.NEGATIVE};
	byte[] record31 = {0,0,2,3,4,5};
	byte[] record41 = {0,0,2,3,4,5,TypeRmComp.POSITIVE};
	byte[] record42 = {0,0,2,3,4,5,TypeRmComp.NEGATIVE};

		
	
	public void testRmPosiveGet() {
		TypeRmCompPositive type = new TypeRmCompPositive();
		
		FieldDetail fld1 = new FieldDetail("", "", Type.ftRmCompPositive, 0, "", 0, "");
		FieldDetail fld2 = new FieldDetail("", "", Type.ftRmCompPositive, 2, "", 0, "");
		fld1.setPosLen(3, 5);
		fld2.setPosLen(3, 5);
		
		//System.out.println("Tst 1 " + type.getField(record3, 2, fld1));
		//System.out.println("Tst 2 " + type.getField(record3, 2, fld2));
		
		assertEquals(" 1 GetValue - Field1 ", "12345",  type.getField(record1, fld1.getPos(), fld1).toString());
		assertEquals(" 2 GetValue - Field2 ", "123.45", type.getField(record1, fld2.getPos(), fld2).toString());
		
		assertEquals(" 3 GetValue - Field1 ", "23456",  type.getField(record2, fld1.getPos(), fld1).toString());
		assertEquals(" 4 GetValue - Field2 ", "234.56", type.getField(record2, fld2.getPos(), fld2).toString());
		
		assertEquals(" 5 GetValue - Field1 ", "23450",  type.getField(record3, fld1.getPos(), fld1).toString());
		assertEquals(" 6 GetValue - Field2 ", "234.50", type.getField(record3, fld2.getPos(), fld2).toString());
	}
		
	
	public void testRmGet() {
		TypeRmComp type = new TypeRmComp();

		FieldDetail fld1 = new FieldDetail("", "", Type.ftRmComp, 0, "", 0, "");
		FieldDetail fld2 = new FieldDetail("", "", Type.ftRmComp, 2, "", 0, "");
		fld1.setPosLen(3, 5);
		fld2.setPosLen(3, 5);
		
		//System.out.println("Tst 1 " + type.getField(record3, 2, fld1));
		//System.out.println("Tst 2 " + type.getField(record3, 2, fld2));
		
		assertEquals(" 1  GetValue - Field1 ", "1234",  type.getField(record11, fld1.getPos(), fld1).toString());
		assertEquals(" 2  GetValue - Field2 ", "12.34", type.getField(record11, fld2.getPos(), fld2).toString());
		
		assertEquals(" 3  GetValue - Field1 ", "2345",  type.getField(record21, fld1.getPos(), fld1).toString());
		assertEquals(" 4  GetValue - Field2 ", "23.45", type.getField(record21, fld2.getPos(), fld2).toString());
		
		assertEquals(" 5  GetValue - Field1 ", "2345",  type.getField(record31, fld1.getPos(), fld1).toString());
		assertEquals(" 6  GetValue - Field2 ", "23.45", type.getField(record31, fld2.getPos(), fld2).toString());

		assertEquals(" 7  GetValue - Field1 ", "-1234",  type.getField(record12, fld1.getPos(), fld1).toString());
		assertEquals(" 8  GetValue - Field2 ", "-12.34", type.getField(record12, fld2.getPos(), fld2).toString());
		
		assertEquals(" 9  GetValue - Field1 ", "-2345",  type.getField(record22, fld1.getPos(), fld1).toString());
		assertEquals(" 10 GetValue - Field2 ", "-23.45", type.getField(record22, fld2.getPos(), fld2).toString());

	}
	
	public void testRmPosiveSet() throws Exception {
		TypeRmCompPositive type = new TypeRmCompPositive();
		
		byte[] record1a = {0,0,0,0,0,0,0,0,0,0};
		byte[] record2a = {0,0,0,0,0,0,0};
		byte[] record3a = {0,0,0,0,0,0,0};

		byte[] record1b = {0,0,0,0,0,0,0,0,0,0};
		byte[] record2b = {0,0,0,0,0,0,0};
		byte[] record3b = {0,0,0,0,0,0,0};
		
		FieldDetail fld1 = new FieldDetail("", "", Type.ftRmCompPositive, 0, "", 0, "");
		FieldDetail fld2 = new FieldDetail("", "", Type.ftRmCompPositive, 2, "", 0, "");
		fld1.setPosLen(3, 5);
		fld2.setPosLen(3, 5);


		checkArray(" 1 SetValue - Field1 ", record1, type.setField(record1a, fld1.getPos(), fld1, "12345"));
		checkArray(" 2 SetValue - Field2 ", record1, type.setField(record1b, fld2.getPos(), fld2, "123.45"));

		checkArray(" 3 SetValue - Field1 ", record2, type.setField(record2a, fld1.getPos(), fld1, "23456"));
		checkArray(" 4 SetValue - Field2 ", record2, type.setField(record2b, fld2.getPos(), fld2, "234.56"));

		checkArray(" 5 SetValue - Field1 ", record4, type.setField(record3a, fld1.getPos(), fld1, "23450"));
		checkArray(" 6 SetValue - Field2 ", record4, type.setField(record3b, fld2.getPos(), fld2, "234.50"));
	}
	
	
	public void testRmSet() throws Exception {
		TypeRmComp type = new TypeRmComp();
		
		byte[] record1a = {0,0,0,0,0,0,0,0,0,0};
		byte[] record2a = {0,0,0,0,0,0,0};
		byte[] record3a = {0,0,0,0,0,0,0};

		byte[] record1b = {0,0,0,0,0,0,0,0,0,0};
		byte[] record2b = {0,0,0,0,0,0,0};
		byte[] record3b = {0,0,0,0,0,0,0};
		
		FieldDetail fld1 = new FieldDetail("", "", Type.ftRmComp, 0, "", 0, "");
		FieldDetail fld2 = new FieldDetail("", "", Type.ftRmComp, 2, "", 0, "");
		fld1.setPosLen(3, 5);
		fld2.setPosLen(3, 5);


		checkArray("  1 SetValue - Field1 ", record11, type.setField(record1a, fld1.getPos(), fld1, "1234"));
		checkArray("  2 SetValue - Field2 ", record11, type.setField(record1b, fld2.getPos(), fld2, "12.34"));

		checkArray("  3 SetValue - Field1 ", record21, type.setField(record2a, fld1.getPos(), fld1, "2345"));
		checkArray("  4 SetValue - Field2 ", record21, type.setField(record2b, fld2.getPos(), fld2, "23.45"));

		checkArray("  5 SetValue - Field1 ", record41, type.setField(record3a, fld1.getPos(), fld1, "2345"));
		checkArray("  6 SetValue - Field2 ", record41, type.setField(record3b, fld2.getPos(), fld2, "23.45"));

		checkArray("  7 SetValue - Field1 ", record12, type.setField(record1a, fld1.getPos(), fld1, "-1234"));
		checkArray("  8 SetValue - Field2 ", record12, type.setField(record1b, fld2.getPos(), fld2, "-12.34"));

		checkArray("  9 SetValue - Field1 ", record22, type.setField(record2a, fld1.getPos(), fld1, "-2345"));
		checkArray(" 10 SetValue - Field2 ", record22, type.setField(record2b, fld2.getPos(), fld2, "-23.45"));

		checkArray(" 11 SetValue - Field1 ", record42, type.setField(record3a, fld1.getPos(), fld1, "-2345"));
		checkArray(" 12 SetValue - Field2 ", record42, type.setField(record3b, fld2.getPos(), fld2, "-23.45"));
	}
	
	private void checkArray(String note, byte[] rec1, byte[] rec2) {
		boolean error = false;
		for (int i = 0; i < rec1.length; i++) {
			if (rec1[i] != rec2[i]) {
				error = true;
				System.out.println("Error: " + note);
				break;
			}
		}
		
		if (error) {
			for (int i = 0; i < rec1.length; i++) {
				System.out.print("\t" + rec1[i]);
			}
			System.out.println();
			for (int i = 0; i < rec2.length; i++) {
				System.out.print("\t" + rec2[i]);
			}
			System.out.println();
			System.out.println();
		}
		assertFalse(note, error);
	}
}
