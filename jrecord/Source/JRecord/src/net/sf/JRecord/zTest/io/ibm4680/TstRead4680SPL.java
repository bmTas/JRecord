package net.sf.JRecord.zTest.io.ibm4680;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;

/**
 * Run through and read the Point of Sale files
 * This will check there are no obvious errors in the
 * record read objects
 * 
 * @author Bruce Martin
 *
 */
public class TstRead4680SPL extends TestCase {
	public void testSPL1() throws Exception {
		tstRead(TstConstants.SAMPLE_DIRECTORY + "Pos_Spl_1.bin", 1291);
	}
	
	public void testSPL2() throws Exception {
		String file = this.getClass().getResource("Pos_Spl_2.bin").getFile();
		tstRead( file, 671);
	}
	
	private void tstRead(String filename, int expected) throws RecordException, Exception {
		String schemaFile = this.getClass().getResource("SPL.Xml").getFile();
		LayoutDetail schema = (new RecordEditorXmlLoader())
									.loadCopyBook(schemaFile, 0, 0, "", 0, 0, null)
									.asLayoutDetail();
		
		//= RecordEditorXmlLoader.getExternalRecord(schemaXml, "Schema").asLayoutDetail();
		AbstractLineReader lineReader = LineIOProvider.getInstance().getLineReader(schema);
		AbstractLine l;
		
		lineReader.open(filename, schema);
		int read = 0, diff = 0, unknown = 0;
		
		while ((l = lineReader.read()) != null) {
			int pref = l.getPreferredLayoutIdx();
			
			if (pref < 0) {
				unknown += 1;
			} else if (l.getData().length == schema.getRecord(pref).getLength() ) {
				read += 1;
			} else {
				diff += 1;
			}
		}
		
		System.out.println(filename + "\t" + unknown + "\t" + read + "\t" + diff);
		lineReader.close();
		assertEquals(0, unknown);
		assertEquals(expected, read);
		assertEquals(0, diff);
	}
}
