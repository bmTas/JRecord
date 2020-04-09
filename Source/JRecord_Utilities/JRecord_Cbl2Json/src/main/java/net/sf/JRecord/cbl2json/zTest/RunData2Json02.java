package net.sf.JRecord.cbl2json.zTest;

import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Data2Json;


public class RunData2Json02 {

	public static void main(String[] args) throws RecordException, IOException,  XMLStreamException {
		String cbl1 =  Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl");
		String forgText = "text";
		String poRec = "PO-Record";
		String recType = "Record-Type";
		String productRec = "Product-Record";
		String locationRec = "Location-Record";
		String poValue = "H1";
		String productValue = "D1";
		String storeValue = "S1";
		String in1 = Cbl2JsonCode.getFullName("Ams_PODownload_20041231.txt");
		String out1 = "G:/Temp/Ams_PODownload_20041231_batch_Tree.json";
		String out2 = "G:/Temp/Ams_PODownload_20041231_batch.json";

			
		String[] args1 = {
					"-cobol", cbl1, "-fileOrganisation", forgText,
					 "-recordSelection", poRec,       recType + "=" + poValue,
					 "-recordSelection", productRec,  recType + "=" + productValue, "-recordParent", productRec, poRec,
					 "-recordSelection", locationRec, recType + "=" + storeValue,   "-recordParent", locationRec, productRec,
					 "-input", in1,
					 "-output", out1,
		};

		Data2Json.main(args1);
		
		String[] args2 = {
					"-cobol", cbl1, "-fileOrganisation", forgText,
					 "-recordSelection", poRec,       recType + "=" + poValue,
					 "-recordSelection", productRec,  recType + "=" + productValue, 
					 "-recordSelection", locationRec, recType + "=" + storeValue,   
					 "-input", in1,
					 "-output", out2,
		};
	
		Data2Json.main(args2);
	}

}
