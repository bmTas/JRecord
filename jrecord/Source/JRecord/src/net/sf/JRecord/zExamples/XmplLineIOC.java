package net.sf.JRecord.zExamples;

import java.util.HashMap;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Reading / writing files using a RecordEditor-XML copybook
 *
 * @author Bruce Martin
 *
 */
public final class XmplLineIOC {


	    /**
	     * Example of
	     * 1) Loading an XML copybook (RecordEditor-Xml)
	     * 2) LineReader / LineWrite classes
	     */
	    private XmplLineIOC() {
	        super();

		    String installDir     = TstConstants.SAMPLE_DIRECTORY;
		    String amsPoFile      = installDir + "Ams_PODownload_20041231.txt";
//		    String salesFileOut   = installDir + "DTAR020out.bin";
		    String copybookName   = TstConstants.RE_XML_DIRECTORY
	    					+ "ams PO Download.Xml";
	        int lineNum = 0;

	        AbstractLine amsPoRecord;

	        try {
	            LayoutDetail schema = CopybookLoaderFactory.getInstance().getLayoutRecordEditXml(copybookName, null);

	            /* with XML copybooks, get the file structure from layout */
	            int fileStructure = schema.getFileStructure();

	            HashMap<String, RecordDetail> recordIdxMap = new HashMap<String, RecordDetail>();
	            recordIdxMap.put("H1", schema.getRecord(schema.getRecordIndex("ams PO Download: Detail")));
	            recordIdxMap.put("D1", schema.getRecord(schema.getRecordIndex("ams PO Download: Header")));
	            recordIdxMap.put("S1", schema.getRecord(schema.getRecordIndex("ams PO Download: Allocation")));



	            AbstractLineReader reader  = LineIOProvider.getInstance().getLineReader(fileStructure);

	            reader.open(amsPoFile, schema);

	            while ((amsPoRecord = reader.read()) != null) {
	            	String recordType = amsPoRecord.getFieldValue("Record Type").asString();
	                lineNum += 1;

	                if (recordIdxMap.containsKey(recordType)) {
	                	RecordDetail recordDetail = recordIdxMap.get(recordType);
		                System.out.println("Line " + lineNum + " Record : " + recordDetail.getRecordName());
		                for (int i = 0; i < recordDetail.getFieldCount(); i++) {
		                	FieldDetail field = recordDetail.getField(i);
							System.out.println(
									  "\t" + field.getName()
									+ "\t\t" + amsPoRecord.getFieldValue(field).asString());
		                }
	                } else {
	                	System.out.println("Invalid Record Type: " + recordType + " at Line Number: " + lineNum
	                			+ amsPoRecord.getFullLine());
	                }

	                System.out.println();
	                System.out.println();
	            }

	            reader.close();
	        } catch (Exception e) {
	            System.out.println("~~> " + lineNum + " " + e.getMessage());
	            System.out.println();

	            e.printStackTrace();
	        }
	    }

	    public static void main(String[] args) {
	    	new XmplLineIOC();
	    }
}
