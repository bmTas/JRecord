package net.sf.JRecord.utilityClasses;

import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.Log.TextLog;

public class SchemaLoader {

	public static ExternalRecord loadSchema(String schemaFileName, int split, String fontname, int binformat) 
	throws Exception  {
		
		CopybookLoader conv;
		if (schemaFileName.toLowerCase().endsWith(".xml")) {
			conv = new RecordEditorXmlLoader();
		} else {
			conv = new CobolCopybookLoader();	
		}
		
		return conv.loadCopyBook(schemaFileName, split, 0, fontname, binformat, 0, new TextLog()); 
	}
}
