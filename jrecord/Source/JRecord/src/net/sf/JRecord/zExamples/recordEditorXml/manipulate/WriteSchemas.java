package net.sf.JRecord.zExamples.recordEditorXml.manipulate;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookWriterManager;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlWriter;
import net.sf.JRecord.Types.Type;

public class WriteSchemas {


	public static void main(String[] args) throws FileNotFoundException, Exception {
		
		ExternalRecord csvInputSchema  = ExternalRecord	
									.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", "\t", "\"") 
									.asExternalRecord();
// Define the output record (with the fields)
		ExternalRecord csvOutputSchema 
				= ExternalRecord
						.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", ";", "\"")
							.addCsvField("Sku",   Type.ftChar, 0)
							.addCsvField("Store", Type.ftNumAnyDecimal, 0)
							.addCsvField("Date",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Dept",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Qty",   Type.ftNumAnyDecimal, 0)
							.addCsvField("Price", Type.ftNumAnyDecimal, 0)
							.addCsvField("GST",   Type.ftNumAnyDecimal, 0)
						.asExternalRecord();

		
    	CopybookWriterManager writerManager = CopybookWriterManager.getInstance();
    	RecordEditorXmlWriter writer
    			= (RecordEditorXmlWriter) writerManager.get(CopybookWriterManager.RECORD_EDITOR_XML_WRITER);

    	writer.writeCopyBook(new FileOutputStream("G:\\Temp\\XML_TabDelimCsvSchema.Xml"), csvInputSchema,  null);
    	writer.writeCopyBook(new FileOutputStream("G:\\Temp\\XML_SaleCsvSchema.Xml"),     csvOutputSchema, null);

	}

}
