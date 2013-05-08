package net.sf.JRecord.zTest.External;

import junit.framework.TestCase;
import net.sf.JRecord.External.DbCsvCopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;

public class TstDbCsvExtractCopybook extends TestCase {

	private final String directory = "/home/bm/Work/RecordEditor/CsvCopybooks/";
	private String copyBookFile = directory + "E_POL.Csv";
	public void testLoadCopyBook() throws Exception {
		DbCsvCopybookLoader cnv = new DbCsvCopybookLoader();
		ExternalRecord rec;
		ExternalField fld;
		

		rec = cnv.loadCopyBook(copyBookFile, 0, 0, "", 0, 0, null);
		
		System.out.println("Number of fields " + rec.getNumberOfRecordFields());
		
		for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
			fld = rec.getRecordField(i);
			System.out.println("  " + fld.getName() + " "
					+ fld.getPos() + " " + fld.getLen() + " > "
					+ fld.getType());
		}

	}

}
