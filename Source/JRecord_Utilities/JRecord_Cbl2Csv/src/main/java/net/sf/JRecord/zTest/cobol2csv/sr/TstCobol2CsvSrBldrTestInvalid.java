package net.sf.JRecord.zTest.cobol2csv.sr;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

import net.sf.JRecord.cbl2csv.imp.sr.FieldConversion;

public class TstCobol2CsvSrBldrTestInvalid {
	private static final String NORMAL_RESULT = 
			"DTAR020-KEYCODE-NO,DTAR020-STORE-NO,DTAR020-DATE,DTAR020-DEPT-NO,DTAR020-QTY-SOLD,DTAR020-SALE-PRICE\n" + 
			"69684558,20,40118,280,1,19.00\n" + 
			"69684558,20,40118,280,-1,-19.00\n" + 
			"69684558,20,40118,280,1,5.01\n" + 
			",****,****,****,****,****\n" + 
			"69694158,20,40118,280,-1,-19.00\n" + 
			"69694158,****,40118,280,1,5.01\n" + 
			"63604808,20,****,170,1,4.87\n" + 
			"62684671,20,40118,****,1,69.99\n" + 
			"62684671,20,40118,685,****,-69.99\n" + 
			"64634429,20,40118,957,1,****\n" + 
			"66624458,20,40118,957,1,0.89\n" + 
			"63674861,20,40118,957,10,2.70\n";
	@Test
	public void testDefault() throws IOException {
		CreateTestData.RunCbl2CsvSrTest tst = new CreateTestData.RunCbl2CsvSrTest(CreateTestData.invalidDtar020Data());
		
		tst.runDefault();
		
		assertEquals(NORMAL_RESULT.replace("****", ""), tst.getResult());
	}

	@Test
	public void testErrorStar() throws IOException {
		CreateTestData.RunCbl2CsvSrTest tst = new CreateTestData.RunCbl2CsvSrTest(CreateTestData.invalidDtar020Data());
		
		tst.fieldConversion = FieldConversion.createFieldConversion(false, "****");
		tst.run();
		
		//System.out.println(tst.getResult());
		
		assertEquals(NORMAL_RESULT, tst.getResult());
		
		tst.csvFieldSeperator = ":";
		tst.run();
		
		assertEquals(NORMAL_RESULT.replace(",", ":"), tst.getResult());

	}

	@Test
	public void testError() throws IOException {
		CreateTestData.RunCbl2CsvSrTest tst = new CreateTestData.RunCbl2CsvSrTest(CreateTestData.invalidDtar020Data());
		int pos = NORMAL_RESULT.indexOf(",****,");
		String expected = NORMAL_RESULT.substring(0, pos);
		boolean failed = false;
		
		tst.fieldConversion = FieldConversion.NORMAL_CONVERSION;
		
		try {
			tst.run();
		} catch (Exception e) {
			failed = true;
		}
		assertEquals(expected, tst.getResult());
		assertTrue("Expection a failure", failed);

	}

}
