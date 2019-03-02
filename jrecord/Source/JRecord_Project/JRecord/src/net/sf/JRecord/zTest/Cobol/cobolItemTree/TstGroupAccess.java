package net.sf.JRecord.zTest.Cobol.cobolItemTree;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.detailsBasic.IItemDetails;

public class TstGroupAccess extends TestCase {
	private static final String USER_CBL
			= "        01  Rec-1.\n"
			+ "            03 User-Name            Pic x(8).\n"
			+ "            03 Birth-Date.\n"
			+ "               05 Birth-Year        Pic 9(4).\n"
			+ "               05 Birth-Month       Pic 99.\n"
			+ "               05 Birth-Day         Pic 99.\n";
	private static final String REC_VALUE_1 = "abcd    20171021";

	public void testGroup01() throws IOException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(new StringReader(USER_CBL), "Rec-1");
		LayoutDetail layout = iob.getLayout();
		List<? extends IItemDetails> cobolItems = layout.getRecord(0).getCobolItems();
		AbstractLine line = iob.newLine();
		
		assertEquals(1, cobolItems.size());
		
		IItemDetails rec1 = cobolItems.get(0);
		IFieldDetail fieldDef = rec1.getFieldDefinition();
		
		for (int i = 0; i < layout.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = layout.getRecord(0).getField(i);
			System.out.println("-- " + field.getName() + "\t" + field.getType());
		}
		
		assertEquals(1, fieldDef.getPos());
		assertEquals(16, fieldDef.getLen());
		
		line.getFieldValue(fieldDef) .set(REC_VALUE_1);
		
		assertEquals(REC_VALUE_1.substring(0, 8).trim(), line.getFieldValue("User-Name").asString());
		assertEquals("2017", line.getFieldValue("Birth-Year").asString());
		assertEquals("10", line.getFieldValue("Birth-Month").asString());
		assertEquals("21", line.getFieldValue("Birth-Day").asString());
		
		assertEquals(2, rec1.getChildItems().size());
		IItemDetails birthDate = rec1.getChildItems().get(1);
		
		IFieldDetail dateDef = birthDate.getFieldDefinition();
		assertEquals(9, dateDef.getPos());
		assertEquals(8, dateDef.getLen());

		assertEquals(REC_VALUE_1.substring(8).trim(), line.getFieldValue(dateDef).asString());
		line.getFieldValue("Birth-Year").set(2015);
		line.getFieldValue("Birth-Month").set(7);
		line.getFieldValue("Birth-Day").set(19);
		assertEquals("20150719", line.getFieldValue(dateDef).asString());
		
		line.getFieldValue(dateDef).set(20120307);
		assertEquals("2012", line.getFieldValue("Birth-Year").asString());
		assertEquals(3, line.getFieldValue("Birth-Month").asInt());
		assertEquals(7, line.getFieldValue("Birth-Day").asInt());
		assertEquals("20120307", line.getFieldValue(dateDef).asString());

	}
}
