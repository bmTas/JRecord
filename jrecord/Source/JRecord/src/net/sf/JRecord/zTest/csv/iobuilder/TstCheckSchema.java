package net.sf.JRecord.zTest.csv.iobuilder;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Types.Type;
import junit.framework.TestCase;

public class TstCheckSchema extends TestCase {
	public void testSchema01() throws Exception {
		ExternalRecord xr;
		
		xr = JRecordInterface1.CSV .newIOBuilder().getExternalRecord();
		assertEquals("\"", xr.getQuote());
		assertEquals(",",  xr.getDelimiter());
		
		xr = JRecordInterface1.CSV .newIOBuilder(";", "`").getExternalRecord();
		assertEquals("`",  xr.getQuote());
		assertEquals(";",  xr.getDelimiter());
	}
	
	public void testSchema02() throws Exception {
		ExternalRecord xr;
		
		xr = JRecordInterface1.CSV 
				.newIOBuilder("|", "'")
					.defineFields()
						.addCsvField("f1", Type.ftChar, 0)
						.addCsvField("f2", Type.ftNumAnyDecimal, 0)
						.addCsvField("f3", Type.ftNumLeftJustified, 2)
						.addCsvField("f4", Type.ftNumRightJustifiedPN, 3)
					.endOfRecord()
				.getExternalRecord();
		assertEquals("'", xr.getQuote());
		assertEquals("|", xr.getDelimiter());
	
		assertEquals(4, xr.getNumberOfRecordFields());
		check(xr.getRecordField(0), 1, "f1", Type.ftChar, 0);                    
		check(xr.getRecordField(1), 2, "f2", Type.ftNumAnyDecimal, 0);
		check(xr.getRecordField(2), 3, "f3", Type.ftNumLeftJustified, 2);
		check(xr.getRecordField(3), 4, "f4", Type.ftNumRightJustifiedPN, 3);
	}
	
	private void check(ExternalField fld, int pos, String n, int type, int decimal) {
		assertEquals(n, fld.getName());
		assertEquals(pos, fld.getPos());
		assertEquals(type, fld.getType());
		assertEquals(decimal, fld.getDecimal());
		assertTrue(fld.getLen() < 0);
	}
}
