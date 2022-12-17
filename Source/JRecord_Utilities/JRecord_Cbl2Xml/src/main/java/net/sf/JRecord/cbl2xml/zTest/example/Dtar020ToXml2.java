package net.sf.JRecord.cbl2xml.zTest.example;


import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.cb2xml.copybookReader.CopybookColumns;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.JRecord.Option.JRecordConstantVars;

public class Dtar020ToXml2 {

    public static void main(String[] args)
    throws IOException, XMLStreamException {
   
        JRecordConstantVars constants = Cobol2Xml.JR_CONSTANTS;
        
        ReadCobolCopybook rc = new ReadCobolCopybook();
        rc	.setColumns(CopybookColumns.STANDARD_COLUMNS)
        	.addCobolCopybook("G:/Users/BruceTst01/RecordEditor_HSQL/CopyBook/Cobol/DTAR020.cbl");
        System.out.println(rc.getFreeFormatCopybookText());
	
        Cobol2Xml.newCobol2Xml(rc.getFreeFormatCopybookReader(), "DTAR020")
        					.setDialect(constants.FMT_MAINFRAME)               
        					.setCopybookFileFormat(Cb2xmlConstants.FREE_FORMAT)

                                         // Cobol Options
        					.setFileOrganization(constants.IO_FIXED_LENGTH)
        					.setSplitCopybook(constants.SPLIT_NONE)      
        					.setFont("cp037")
  

              .cobol2xml("G:/Users/BruceTst01/RecordEditor_HSQL/SampleFiles/DTAR020.bin", 
                         "G:/Users/BruceTst01/RecordEditor_HSQL/SampleFiles/DTAR020.bin.xml");
    }
}

