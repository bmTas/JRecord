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
import net.sf.JRecord.IO.TextLineWriter;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.cb2xml.def.Cb2xmlConstants;


/**
 * Writing an ebcidic file using \n line (record) separators
 * Using TextLineWriter
 * 
 * @author Bruce Martin
 *
 */
public class XmplEbcdicWriter01 {

	static String charset = "cp037";
	static String cobolCopyBook = "        01  A-Line     Pic x(40).";
	
	public static void main(String[] args)  {
		String fileName =  TstConstants.TEMP_DIRECTORY
                + "cp037FixedWidth_01.txt";
		try {
			ExternalRecord schemaBldr = getCobolLayout(ICopybookDialects.FMT_MAINFRAME, cobolCopyBook);
			byte[] eolBytes = "\n".getBytes(charset);
			schemaBldr.setRecordSep(eolBytes);
			schemaBldr.setRecSepList(Constants.CR_STRING);
			
			LayoutDetail schema = schemaBldr.asLayoutDetail();
			AbstractLineWriter w = new TextLineWriter(false);

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


		return loader.loadCopyBook(
				bs, "ZZ-TTT4", CopybookLoader.SPLIT_NONE, 0, charset, Cb2xmlConstants.USE_STANDARD_COLUMNS,
				cobolDialect, 0, new TextLog());
	}
}
