package net.sf.JRecord.zExamples.cobol.ebcdicUnix;

import java.io.ByteArrayInputStream;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Writing an ebcidic file using \n line (record) separators
 * Using a LineIOProvider
 * 
 * @author Bruce Martin
 *
 */

public class XmplEbcdicWriter04 {

	static String charset = "cp037";
	static String cobolCopyBook = "        01  A-Line     Pic x(40).";
	
	public static void main(String[] args)  {
		String fileName =  TstConstants.TEMP_DIRECTORY
                + "cp037FixedWidth_04.txt";
		try {
			ExternalRecord schemaBldr = getCobolLayout(Convert.FMT_MAINFRAME, cobolCopyBook);
			byte[] eolBytes = "\n".getBytes(charset);
			schemaBldr.setRecordSep(eolBytes);
			schemaBldr.setRecSepList(Constants.CR_STRING);
			
			LayoutDetail schema = schemaBldr.asLayoutDetail();
			AbstractLineWriter w = LineIOProvider.getInstance().getLineWriter(Constants.IO_TEXT_LINE);

			System.out.println("Output File: " + fileName);
			w.open(fileName);
			
			for (int i = 1; i <120; i++) {
				w.write(new Line(schema, "Line " + i));
			}
			w.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}
	
	
	private static ExternalRecord getCobolLayout(int cobolDialect, String cobolCopybook) throws RecordException {
		CobolCopybookLoader loader = new CobolCopybookLoader();
		ByteArrayInputStream bs = new ByteArrayInputStream(cobolCopybook.getBytes());


		return loader.loadCopyBook(bs, "ZZ-TTT4", CopybookLoader.SPLIT_NONE, 0, charset, cobolDialect, 0, new TextLog());
	}
}
