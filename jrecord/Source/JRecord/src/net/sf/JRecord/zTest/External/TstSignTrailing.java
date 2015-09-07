package net.sf.JRecord.zTest.External;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

public class TstSignTrailing extends TestCase {
	private static final String CPYBOOK1
				= "         05 group.\n"
				+ "            07 num1               pic 9(4)-.\n"
				+ "            07 num2               pic 999.99-.\n"
				+ "            07 num3               pic 9(5).99-.\n";
	
	public void test1() throws RecordException, IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(CPYBOOK1.getBytes()), "Copybook");
		ExternalRecord rec = ioBldr.getExternalRecord();
		LayoutDetail schema = rec.asLayoutDetail();
		AbstractLine line = new Line(schema);
		
		for (int i = 0 ; i < rec.getNumberOfRecordFields(); i++) {
			assertEquals(Type.ftSignSeparateTrail, rec.getRecordField(i).getType());
			System.out.println(rec.getRecordField(i).getName() + "\t" + rec.getRecordField(i).getType() + " " + Type.ftSignSeparateTrail
					+ " " + TypeManager.isNumeric(Type.ftSignSeparateTrail));
		}
		
		RecordDetail r = schema.getRecord(0);
		for (int i = 0; i < r.getFieldCount(); i++) {
			assertEquals(Type.ftSignSeparateTrail, r.getField(i).getType());
		}
		
		line.getFieldValue(0,0).set(-100);
		line.getFieldValue(0,1).set(-121);
		line.getFieldValue(0,2).set(-131);
		assertTrue(line.getFieldValue(0,0).isNumeric());
		assertTrue(line.getFieldValue(0,1).isNumeric());
		assertTrue(line.getFieldValue(0,2).isNumeric());
		System.out.println(line.getFullLine());
		
		assertEquals("0100-121000-00131000-", line.getFullLine());
		line.getFieldValue(0,0).set(100);
		line.getFieldValue(0,1).set(121);
		line.getFieldValue(0,2).set(131);

		System.out.println(line.getFullLine());
		
		//assertEquals("0100 121000 00131000 ", line.getFullLine());
	}
}
