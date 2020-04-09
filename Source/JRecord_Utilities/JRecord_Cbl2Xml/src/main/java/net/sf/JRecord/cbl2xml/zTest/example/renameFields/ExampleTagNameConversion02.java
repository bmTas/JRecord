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
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.Cb2XmlCode;

public class ExampleTagNameConversion02 {
    public static void main(String[] args) 
    throws IOException, XMLStreamException {
   
        JRecordConstantVars constants = Cobol2Xml.JR_CONSTANTS;
        ByteArrayOutputStream cblData = new ByteArrayOutputStream();
	
        String dtar020FileName = Cb2XmlCode.getFullName("cobol/DTAR020.cbl");
        
        RenameFieldsMap rf = new RenameFieldsMap()
           		.add("DTAR020-KEYCODE-NO", "KEYCODE-NO")
           		.add("DTAR020-STORE-NO",   "STORE-NO")
           		.add("DTAR020-DATE", "DATE")
           		.add("DTAR020-DEPT-NO", "DEPT-NO")
           		.add("DTAR020-QTY-SOLD", "QTY-SOLD")
          		.add("DTAR020-SALE-PRICE", "SALE-PRICE")
          		.add("DTAR020-KCODE-STORE-KEY", "KCODE-STORE-KEY")
           ;
        
		ICobol2Xml cbl2Xml = Cobol2Xml.newCobol2Xml(dtar020FileName)

                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("cp037")
                         .setRenameFieldClass(rf);
		
		cbl2Xml.xml2Cobol(
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
        
        ByteArrayOutputStream xmlOut = new ByteArrayOutputStream(cblData.size() * 25);
        cbl2Xml.cobol2xml(new ByteArrayInputStream(cblData.toByteArray()), xmlOut);
        System.out.println();
        System.out.println(new String(xmlOut.toByteArray()));
    }
 
	
}
