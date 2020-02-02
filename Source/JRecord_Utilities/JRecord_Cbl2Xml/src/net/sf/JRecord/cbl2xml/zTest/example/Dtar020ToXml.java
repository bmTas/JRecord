package net.sf.JRecord.cbl2xml.zTest.example;


import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.Option.JRecordConstantVars;

public class Dtar020ToXml {

    public static void main(String[] args)
    throws IOException, XMLStreamException {
   
        JRecordConstantVars constants = Cobol2Xml.JR_CONSTANTS;
	
        Cobol2Xml.newCobol2Xml("G:/Users/Bruce01/RecordEditor_HSQL/CopyBook/Cobol/DTAR020.cbl")


                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("cp037")

              .cobol2xml("G:/Users/Bruce01/RecordEditor_HSQL/SampleFiles/DTAR020.bin", 
                         "G:/Users/Bruce01/RecordEditor_HSQL/SampleFiles/DTAR020.bin.xml");
    }
}

