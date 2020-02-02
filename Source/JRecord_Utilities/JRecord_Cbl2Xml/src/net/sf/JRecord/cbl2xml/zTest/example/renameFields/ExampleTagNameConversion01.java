package net.sf.JRecord.cbl2xml.zTest.example.renameFields;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.FieldIterator;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;
import net.sf.JRecord.fieldNameConversion.IRenameField;

public class ExampleTagNameConversion01 {
    public static void main(String[] args) 
    throws IOException, XMLStreamException {
   
        JRecordConstantVars constants = Cobol2Xml.JR_CONSTANTS;
        ByteArrayOutputStream cblData = new ByteArrayOutputStream();
	
        String dtar020FileName = Cb2XmlCode.getFullName("cobol/DTAR020.cbl");
        
        
		Cobol2Xml.newCobol2Xml(dtar020FileName)

                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("cp037")
                         .setRenameFieldClass(new DropDTAR020FromName())

              .xml2Cobol(
            		  		new FileInputStream(Cb2XmlCode.getFullName("xml/DTAR020_DataY.xml")),
            		  		cblData	
            		  	);
		
        AbstractLineReader reader = JRecordInterface1.COBOL.newIOBuilder(dtar020FileName)
        
			        .setFileOrganization(constants.IO_FIXED_LENGTH)
			        .setDialect(constants.FMT_MAINFRAME)               
			        .setSplitCopybook(constants.SPLIT_NONE)      
			        .setFont("cp037")
		        .newReader(new ByteArrayInputStream(cblData.toByteArray()));
        AbstractLine l;
        
        while( (l = reader.read()) != null) {
        	FieldIterator fieldIterator = l.getFieldIterator(0);
        	for(AbstractFieldValue val : fieldIterator) {
        		System.out.print(val.getFieldDetail().getName() + ": " + val.asString() + "\t");
        	}
        	System.out.println();
        }
        reader.close();
    }
    
    private static class DropDTAR020FromName implements IRenameField {

		private static final String COPYBOOK_NAME = "DTAR020-";

		@Override
		public String toFieldName(String schemaName) {
			if (schemaName.startsWith(COPYBOOK_NAME)) {
				schemaName = schemaName.substring(COPYBOOK_NAME.length());
			}
					
			return schemaName;
		}
    	
    }
	
}
