package net.sf.JRecord.zExamples;

import java.io.IOException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Common.XmlConstants;
import net.sf.JRecord.CsvParser.ParserManager;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.XmlLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Example of Generic Copy procedure. This procedure
 * can read any or Binary / Flat / CSV / XML and write out
 * any of Binary / Flat / CSV / XML.
 * 
 * The program demistrates that reading / writing files
 * is largely independent of the format.
 * 
 */
public final class XmplLineIO9 {


	    /**
	     * Example of Generic Copy procedure (copy To / From Binary / Flat / CSV / XML). This procedure
	     * can read any of Binary / Flat / CSV / XML and write out
	     * any of Binary / Flat / CSV / XML.
	     * 
	     * The program deminstrates that reading / writing files
	     * is largely independent of the format.
	     * 
	     */
	    private XmplLineIO9() {
	        super();

		    String installDir    = TstConstants.SAMPLE_DIRECTORY;
		    String xmlDir        = TstConstants.RE_XML_DIRECTORY;

		    String salesFile1    = installDir + "DTAR020.bin";
		    String salesFile2    = installDir + "DTAR020.csv";
		    String salesFile3    = installDir + "Xml_DTAR020.xml";
		    
		    String salesFileOut1 = installDir + "DTAR020out9a.bin";
		    String salesFileOut2 = installDir + "DTAR020out9b.bin";
		    String salesFileOut3 = installDir + "DTAR020out9c.bin";
		    
		    String salesFileOut4 = installDir + "xml_DTAR020out9d.xml";
		    String salesFileOut5 = installDir + "csv_DTAR020out9e.csv";

	    
		    String salesFileOut5a = installDir + "csv_DTAR020out9e_Quote1.csv";
		    String salesFileOut6 = installDir + "csv_DTAR020out9f_Quote1.csv";
		    
		    String stdCopyBook   = xmlDir + "DTAR020.Xml";
		    String csvCopyBook1  = xmlDir + "Tab Delimited names on the first line.Xml";
		    
		    String xmlCopybook3  = xmlDir + "XML - Build Layout.Xml";
		    String xmlCopybook4  = xmlDir + "Xml_DTAR020_Copybook.Xml";
		    String csvCopybook5  = xmlDir + "csv_DTAR020.Xml";
		    String csvCopybook6  = xmlDir + "csv_DTAR020_quote.Xml";
		    
		    
	        int lineNum = 0;

	        try {
	            CopybookLoader loader  = new RecordEditorXmlLoader();

	            ExternalRecord layout  = loader.loadCopyBook(stdCopyBook,  0, 0, "", 0, 0, null);
	            ExternalRecord layout2 = loader.loadCopyBook(csvCopyBook1, 0, 0, "", 0, 0, null);
	            ExternalRecord layout3 = loader.loadCopyBook(xmlCopybook3, 0, 0, "", 0, 0, null);
	            ExternalRecord layout4 = loader.loadCopyBook(xmlCopybook4, 0, 0, "", 0, 0, null);
	            ExternalRecord layout5 = loader.loadCopyBook(csvCopybook5, 0, 0, "", 0, 0, null);
	            ExternalRecord layout6 = loader.loadCopyBook(csvCopybook6, 0, 0, "", 0, 0, null);
	            
	            copyFile(layout,  salesFile1, layout,  salesFileOut1); /* Binary to Binary */
	            copyFile(layout2, salesFile2, layout,  salesFileOut2); /* Csv    to Binary */
	            copyFile(layout3, salesFile3, layout,  salesFileOut3); /* Xml    to Binary */
	            copyFile(layout,  salesFile1, layout4, salesFileOut4); /* Binary to XML    */
	            copyFile(layout,  salesFile1, layout5, salesFileOut5); /* Binary to Csv    */
	            copyFile(layout,  salesFile1, layout6, salesFileOut6); /* Binary to Csv    */
	            
	            layout5.setRecordStyle(ParserManager.DB_CSV_PARSER);
	            layout5.setQuote("'");
	            
	            copyFile(layout,  salesFile1, layout5, salesFileOut5a);
	        } catch (Exception e) {
	            System.out.println("~~> " + lineNum + " " + e.getMessage());
	            System.out.println();

	            e.printStackTrace();
	        }
	    }
	    
	    /**
	     * Generic File copy. This routine will copy one file to another.
	     * The 2 files can be of different layouts (could be Binary / Text / CSV or XML).
	     * The one limitation is there can be only one Data layout
	     * 
	     * @param inLayout  record layout of the input file
	     * @param inFile    input file
	     * @param outLayout record layout of the output file
	     * @param outFile   output file
	     * @throws IOException any IO error that occurs
	     * @throws RecordException any JRecord Error
	     */
	    public void copyFile(ExternalRecord inLayout,  String inFile, 
	    					 ExternalRecord outLayout, String outFile)
	    throws IOException, RecordException {
	    	int i, pref, count, outIdx;
	    	AbstractLine inLine, outLine;
            LayoutDetail iLayout = inLayout.asLayoutDetail();
            LayoutDetail oLayout = outLayout.asLayoutDetail();
            AbstractLineReader reader  = LineIOProvider.getInstance().getLineReader(iLayout.getFileStructure());
            AbstractLineWriter writer  = LineIOProvider.getInstance().getLineWriter(oLayout.getFileStructure());
            
            String xmlType = null;

            reader.open(inFile, iLayout);
            writer.open(outFile);
            
            /* 
             * Create the output Line 
             *  - For XML use XmlLine (and setup XML Constants
             *  - Otherwise use a Line 
             */
            if (oLayout.isXml()) {
            	int dataIdx = 0;
            	int firstLayout = Integer.MAX_VALUE;
            	for (i = 0; i < oLayout.getRecordCount(); i++) {
            		if ((!  oLayout.getRecord(i).getRecordName().toUpperCase().startsWith("XML")) 
            		&&   !  oLayout.getRecord(i).getRecordName().startsWith("/")){
            			firstLayout = Math.min(firstLayout, i);
             			dataIdx = i;
            		}
            	}
            	
            	
            	if (firstLayout != dataIdx && firstLayout < Integer.MAX_VALUE) {
            		xmlType = oLayout.getRecord(firstLayout).getRecordName();
                	outLine = new XmlLine(oLayout, firstLayout);
                	outLine.getFieldValue(XmlConstants.END_ELEMENT).set("false");
                	outLine.getFieldValue(XmlConstants.XML_NAME).set(xmlType);
                	writer.write(outLine);
            	}
            	String recordToWrite = oLayout.getRecord(dataIdx).getRecordName();
            	outLine = new XmlLine(oLayout, dataIdx);
            	outLine.getFieldValue(XmlConstants.END_ELEMENT).set("true");
            	outLine.getFieldValue(XmlConstants.XML_NAME).set(recordToWrite);
            } else {
            	outLine = new Line(oLayout);
            }
            
            iLayout = reader.getLayout(); /* get The layout from the reader           */
            							  /* incase it has been update (ie CSV / XML) */

            count = 0;
            
            if (iLayout.isXml()) {
	            while ((inLine = reader.read()) != null) {
	            	pref = Math.max(0, inLine.getPreferredLayoutIdx());
	            	RecordDetail rec = iLayout.getRecord(pref);
	            	
	            	outIdx = oLayout.getRecordIndex(rec.getRecordName());
	            	
	            	/* Check if there is a matching output record */
	            	if (outIdx >= 0 && pref >= 0)  {                  	
	                   	/* Loop through the fields */
		            	for (i = 0; i < rec.getFieldCount(); i++) {
			           	//for (i = rec.getFieldCount()-1; i >= 0; i--) {
		            		try {
		            			outLine.getFieldValue(rec.getField(i).getName()).set(
		            					inLine.getField(pref, i)
		            			);
							} catch (Exception e) {	}
		            	}
		            	count += 1;
		                writer.write(outLine);
	            	}
	            }
	            //System.out.println();
            } else {
	            while ((inLine = reader.read()) != null) {
	            	pref = Math.max(0, inLine.getPreferredLayoutIdx());
	            	RecordDetail rec = iLayout.getRecord(pref);
                   	
                   	/* Loop through the fields */
	            	//for (i = rec.getFieldCount()-1; i >= 0; i--) {
	            	for (i = 0; i < rec.getFieldCount(); i++) {
	            		try {
	            			outLine.getFieldValue(rec.getField(i).getName()).set(
	            					inLine.getField(pref, i)
	            			);
						} catch (Exception e) {	}
	            	}
	            	count += 1;
	                writer.write(outLine);
	            }
            }
            
            if (xmlType != null) {
            	outLine.getFieldValue(XmlConstants.XML_NAME).set("/" + xmlType);
            	writer.write(outLine);
            }

            System.out.println( count + " records where copied from " + inFile  
            		+ " " + iLayout.getFileStructure() + " " + inLayout.getFileStructure());
            System.out.println(      "                           to " + outFile
            		+ " " + oLayout.getFileStructure() + " " + outLayout.getFileStructure());
            reader.close();
            writer.close();
	    }

	    public static void main(String[] args) {
	    	new XmplLineIO9();
	    }
}
