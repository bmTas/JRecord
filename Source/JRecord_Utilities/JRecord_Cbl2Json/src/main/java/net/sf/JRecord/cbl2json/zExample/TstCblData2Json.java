package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;
import java.net.URL;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.schema.ArrayElementChecks;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

/**
 * 
 * @author Bruce Martin
 *
 */
public class TstCblData2Json {

    public static String getFullName(String filename) {
    	URL resource = TstCblData2Json.class.getResource(filename);
    	if (resource == null) {
    		System.out.println(" --> Can not find: " + filename);
    	}
		return resource.getFile();
    }


	public static void main(String[] args) throws RecordException, IOException,  XMLStreamException {
		ICobol2Json cbl2json = Cobol2Json.newCobol2Json(getFullName("cbl2xml_Test112.cbl"));
		cbl2json.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		
		cbl2json.cobol2json(getFullName("StoreSales5.txt"), "G:/Temp/Store_Sales_5a.json");
		
		cbl2json	.setArrayCheck("A-Sale", ArrayElementChecks.INSTANCE.newSkipSpacesZeros())
				.setArrayCheck("Department-Dtls", ArrayElementChecks.INSTANCE.newSkipSpacesZeros())
				.setArrayCheck("Product-details", ArrayElementChecks.INSTANCE.newSkipSpacesZeros())
				.setArrayCheck("Orders", ArrayElementChecks.INSTANCE.newStopAtSpaces())
			.cobol2json(getFullName("StoreSales5.txt"), "G:/Temp/Store_Sales_5b.json");
		
		//cbl2xml.xml2Cobol(getFullName("StoreSales5.xml"), "G:/Temp/Store_Sales_5.txt");

	}
}
