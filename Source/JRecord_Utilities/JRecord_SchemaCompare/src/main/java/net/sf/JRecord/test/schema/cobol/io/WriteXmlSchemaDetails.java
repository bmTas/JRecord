package net.sf.JRecord.test.schema.cobol.io;

import java.io.File;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.def.IO.builders.IIOBuilder;


/**
 * Purpose:  This program will convert every Cobol-Copybook in a 
 *         List of FileNames in to JRecord-Layouts. These Layouts are
 *         written to a Text File.
 *           Typically the File List is all the files in a Directory
 * 
 * @author Bruce Martin
 *
 *
 */
public class WriteXmlSchemaDetails {

	public WriteXmlSchemaDetails(IParms parms) throws IOException {
		String[] filenames = parms.getFileNames();
		int dialect = parms.getDialect();
		
		WriteJrSchema writer = new WriteJrSchema(parms.getOutputFileName());
		
		for (String fn : filenames) {
			String filename = new File(fn) .getName();
			try {
				IIOBuilder iob = JRecordInterface1.SCHEMA_XML
							.newIOBuilder(fn);
				LayoutDetail l = iob.getLayout();
				
				writer.writeCopybookHeaderRecord(filename, "Y");
				writer.writeSchema(dialect, l);
			} catch (Exception e) {
				writer.writeCopybookHeaderRecord(filename, "N");
				e.printStackTrace();
			}
		}
		
		writer.close();
	}
}
