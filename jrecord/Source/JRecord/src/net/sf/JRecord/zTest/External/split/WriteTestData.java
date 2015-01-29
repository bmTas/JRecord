package net.sf.JRecord.zTest.External.split;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;


/**
 *                   
 * @author Bruce Martin
 *
 */
public class WriteTestData {

	private LayoutDetail schema;
	
    public static void main(String[] args) throws Exception {
    	new WriteTestData();
    }
    
    
    private WriteTestData() throws Exception {
    	writeRecordDefinition("example.cbl");
 /*   	loadRecordDefinition("example1.cbl");
    	loadRecordDefinition("example2.cbl");
       	loadRecordDefinition("example3.cbl");*/
   	
    	//readFile();
    }
    
    
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private void writeRecordDefinition(String copybook) throws Exception{
    	
    	String copyName = this.getClass().getResource(copybook).getFile();

    	CobolCopybookLoader loaderXML = new CobolCopybookLoader();

    	ExternalRecord extlayoutCBL = loaderXML.loadCopyBook(
    			copyName , CopybookLoader.SPLIT_HIGHEST_REPEATING, 0,
				/* Font name */"", Convert.FMT_MAINFRAME, 0, new TextLog());
    	
    	schema = extlayoutCBL.asLayoutDetail();
    	
    	System.out.println();
    	System.out.print("Schema: " + copybook + " Record Count:" +schema.getRecordCount());
    	
    	for (int i = 0; i < schema.getRecordCount(); i++) {
    		RecordDetail record = schema.getRecord(i);
    		System.out.println("\t), ");
			System.out.print("\t new RecordDtls(\"" + record.getRecordName() + "\", ");
			String sep = "  ";
			for (int j = 0; j < record.getFieldCount(); j++) {
				FieldDetail field = record.getField(j);
				if (j % 3 == 0) {
					System.out.println();
					System.out.print("\t\t");
				}
				System.out.print(sep + "new FieldDtls(\"" + field.getName() + "\", " + field.getPos() + ", " + field.getLen()
						+ ", " + field.getType()+ ")" );
				sep = ", ";
			}
    		System.out.println();
    	}
    }    
 }
