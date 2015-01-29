package net.sf.JRecord.zExamples.recordEditorXml.manipulate;

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookWriterManager;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlWriter;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;


/**
 * Purpose: This program demonstrates converting a Cobol Copybook to a
 *     Xml copybook.
 *
 * @author Bruce Martin
 *
 */
public class ConvertCbl2RecordEditorXml {

	private static byte[] copyBookBytes
			= (
				  "              03  DTAR020-KCODE-STORE-KEY.\n"
				+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).\n"
				+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3.\n"
				+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3.\n"
				+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3.\n"
				+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3.\n"
				+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3.\n"
			).getBytes();

    public static void main(String[] args) throws Exception {
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    	    new ByteArrayInputStream(copyBookBytes),
    	    Conversion.getCopyBookId("DTAR020.cbl"),
    	    CopybookLoader.SPLIT_NONE, 0, "", Convert.FMT_INTEL, 0, new TextLog());
    	CopybookWriterManager writerManager = CopybookWriterManager.getInstance();
    	RecordEditorXmlWriter writer
    			= (RecordEditorXmlWriter) writerManager.get(CopybookWriterManager.RECORD_EDITOR_XML_WRITER);

    	writer.writeCopyBook(new FileOutputStream("H:\\Temp\\XML_DTAR020.Xml"), extlayoutCBL, null);
    }
}
