package net.sf.JRecord.cbl2xml.zTest.example;
import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.Option.JRecordConstantVars;

public class InvoiceFromXml {

    public static void main(String[] args) 
    throws IOException, XMLStreamException {
   
        JRecordConstantVars constants = Cobol2Xml.JR_CONSTANTS;
	
        Cobol2Xml.newCobol2Xml("F:/Work/CobolToXml/Build/example/in/invoice.cbl")

                                         // Cobol Options
                         .setFileOrganization(constants.IO_TEXT_LINE)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE) 

              .xml2Cobol("F:/Work/CobolToXml/Build/example/out/xxx.xml",
            		  "F:/Work/CobolToXml/Build/example/in/invoice.txt");
    }
}
