package net.sf.JRecord.zTest.cobol2csv.sr;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

public class TstCobol2CsvSrBldr {
	private static final String NORMAL_RESULT = 
				"69684558,20,40118,280,1,19.00\n" + 
				"69684558,20,40118,280,-1,-19.00\n" + 
				"69684558,20,40118,280,1,5.01\n" + 
				"69694158,20,40118,280,1,19.00\n" + 
				"69694158,20,40118,280,-1,-19.00\n" + 
				"69694158,20,40118,280,1,5.01\n" + 
				"63604808,20,40118,170,1,4.87\n" + 
				"62684671,20,40118,685,1,69.99\n" + 
				"62684671,20,40118,685,-1,-69.99\n" + 
				"64634429,20,40118,957,1,3.99\n" + 
				"66624458,20,40118,957,1,0.89\n" + 
				"63674861,20,40118,957,10,2.70\n" + 
				"";	
	private static final String NORMAL_RESULT_WITH_COLUMN_HEADINGS = 
				"DTAR020-KEYCODE-NO,DTAR020-STORE-NO,DTAR020-DATE,DTAR020-DEPT-NO,DTAR020-QTY-SOLD,DTAR020-SALE-PRICE\n" + 
				NORMAL_RESULT;


	@Test
	public void testDefault() throws IOException {
		CreateTestData.RunCbl2CsvSrTest tst = new CreateTestData.RunCbl2CsvSrTest(CreateTestData.validDtar020Data());
		
		tst.runDefault();
		
		assertEquals(NORMAL_RESULT_WITH_COLUMN_HEADINGS, tst.getResult());
		
		tst.run();
		assertEquals(NORMAL_RESULT_WITH_COLUMN_HEADINGS, tst.getResult());
	}

	@Test
	public void testSemiSep() throws IOException {
		CreateTestData.RunCbl2CsvSrTest tst = new CreateTestData.RunCbl2CsvSrTest(CreateTestData.validDtar020Data());
		
		tst.csvFieldSeperator = ";";
		tst.run();
		assertEquals(NORMAL_RESULT_WITH_COLUMN_HEADINGS.replace(',', ';'), tst.getResult());
	}
	

	@Test
	public void testNoHeadings() throws IOException {
		CreateTestData.RunCbl2CsvSrTest tst = new CreateTestData.RunCbl2CsvSrTest(CreateTestData.validDtar020Data());
		
		//tst.csvFieldSeperator = ";";
		tst.columnHeadings = false;
		tst.run();
		assertEquals(NORMAL_RESULT, tst.getResult());
		
		tst.csvFieldSeperator = "\t";
		tst.run();
		assertEquals(NORMAL_RESULT.replace(',', '\t'), tst.getResult());
	}

}
