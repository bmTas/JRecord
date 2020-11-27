package net.sf.JRecord.cbl2json.zTest;

import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Data2Json;

public class RunData2Json01 {

	public static void main(String[] args) throws RecordException, IOException,  XMLStreamException {
		String[] a = {
				"-cobol", Cbl2JsonCode.getFullName("DTAR020.cbl"), "-font", "cp037", 
				"-fileOrganisation", "FixedWidth", "-input", Cbl2JsonCode.getFullName("DTAR020.bin"),
				"-output", "G:/Temp/DTAR020_A.json"
		};
 
		Data2Json.main(a);
		String[] a1 = {
				"-cobol", Cbl2JsonCode.getFullName("DTAR020.cbl"), "-font", "cp037", 
				"-fileOrganisation", "FixedWidth", "-input", Cbl2JsonCode.getFullName("DTAR020_Tst1.bin"),
				"-output", "G:/Temp/DTAR020_B.json"
		};
 
		Data2Json.main(a1);
	}

}
