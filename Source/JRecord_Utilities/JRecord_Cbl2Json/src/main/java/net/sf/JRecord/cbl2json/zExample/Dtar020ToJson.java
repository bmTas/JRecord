package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.cobolToJson.Cobol2Json;
	
public class Dtar020ToJson {

    public static void main(String[] args)
    throws IOException,  XMLStreamException {
   
        JRecordConstantVars constants = Cobol2Json.JR_CONSTANTS;
	
        Cobol2Json.newCobol2Json("G:/Users/Bruce01/RecordEditor_HSQL/CopyBook/Cobol/DTAR020.cbl")

                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("cp037")

              .cobol2json("G:/Users/Bruce01/RecordEditor_HSQL/SampleFiles/DTAR020.bin", 
                          "G:/Users/Bruce01/RecordEditor_HSQL/SampleFiles/DTAR020.bin.json");
    }
}
