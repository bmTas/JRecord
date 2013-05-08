package net.sf.JRecord.zTest.External;

import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CsvNamesFirstLineFileLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.zTest.Common.IO;
import net.sf.JRecord.zTest.Common.TstConstants;

public class TstCsvName1stLineLoader extends TestCase {
	private String[] names = {"Brand Id","Loc Nbr","Loc Type","Loc Name","Loc Addr Ln1","Loc Addr Ln2",
				"Loc Addr Ln3","Loc Postcode","Loc State","Loc Actv Ind",
	};
	private String copybookName = TstConstants.CSV_DIRECTORY_OUTPUT + "csvCopybook.Txt";

	
	public void testLoadCopyBook1() throws IOException, RecordException {
		tst("\t");
	}
	
	
	public void testLoadCopyBook2() throws IOException, RecordException {
		tst(",");
	}

	
	private void tst(String sep) throws IOException, RecordException  {
		CsvNamesFirstLineFileLoader l = new CsvNamesFirstLineFileLoader(sep);
		TextLog log = new TextLog();
		writeFile(sep, copybookName);
		
		ExternalRecord r = l.loadCopyBook(copybookName, 0, 0, "", 0, 0, log);
		
		assertEquals("Wrong number of records 1", 1, r.getNumberOfRecords());

		
		r = r.getRecord(0);
		assertEquals("Wrong number of records 0", 0, r.getNumberOfRecords());
		assertEquals("Wrong number of fields ", names.length, r.getNumberOfRecordFields());
		
		for (int i = 0; i < names.length; i++) {
			assertEquals("Wrong Field name " + i , names[i], r.getRecordField(i).getName());
		}
	}

	
	private void writeFile(String seperator, String filename) throws IOException {
		String sep = "";
		StringBuffer b = new StringBuffer();
		
		for (int i = 0; i < names.length; i++) {
			b.append(sep).append(names[i]);
			sep = seperator;
		}
		String[] l = new String[] {b.toString()};
		
		IO.writeAFile(filename, l, "\n");		
		
	}
}
