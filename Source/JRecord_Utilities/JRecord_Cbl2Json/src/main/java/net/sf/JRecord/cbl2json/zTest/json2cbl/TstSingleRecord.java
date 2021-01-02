package net.sf.JRecord.cbl2json.zTest.json2cbl;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.cobolToJson.def.ICobol2Json;

public class TstSingleRecord {

	@Test
	public void testToJson() throws IOException {
		ICobol2Json c2j = Data_AmsPo.getAmsC2J();
		int i = 0;
		for (String s : Data_AmsPo.PO_LINES_1) {
			String jsonStr = c2j.singleCobolRecord2jsonString(s.getBytes());
			assertEquals("Line: " + i, Data_AmsPo.PO_JSON_LINES_1[i++], stripSR(jsonStr));
		}
	}
	@Test
	public void testFromJson() throws IOException {
		ICobol2Json c2j = Data_AmsPo.getAmsC2J();
		int i = 0;
		for (String s : Data_AmsPo.PO_JSON_LINES_1) {
			String jsonStr = new String(c2j.jsonStringToSingleCobolRecord(s));
			//System.out.println("~~ " + Data_AmsPo.PO_LINES_1[i].length() + " " + jsonStr.length());
			assertEquals("Line: " + i, Data_AmsPo.PO_LINES_1[i++], jsonStr);
		}
	}

	@Test
	public void testToJsonDTAR020() throws IOException {
		ICobol2Json c2j = Data_DTAR.getDTAR020C2J();
		ISchemaIOBuilder ioBuilder = c2j.asIOBuilder();
		for (int i = 0; i < Data_DTAR.DTAR020_FIELDS_1.length; i++) {
			AbstractLine l = ioBuilder.newLine();
			for (int j = 0; j < Data_DTAR.DTAR020_FIELDS_1[i].length; j++) {
				l.getFieldValue(0, j).set(Data_DTAR.DTAR020_FIELDS_1[i][j]);
			}
			String jsonStr = c2j.singleCobolRecord2jsonString(l.getData());
			assertEquals("Line: " + i, Data_DTAR.DTAR020_JSON_LINES_1[i], stripSR(jsonStr));
		}
	}

	@Test
	public void testFromJsonDTAR020() throws IOException {
		ICobol2Json c2j = Data_DTAR.getDTAR020C2J();
		ISchemaIOBuilder ioBuilder = c2j.asIOBuilder();
		int i = 0;
		for (String s : Data_DTAR.DTAR020_JSON_LINES_1) {
			AbstractLine l = ioBuilder.newLine(c2j.jsonStringToSingleCobolRecord(s));
			//System.out.println("~~ " + Data_AmsPo.PO_LINES_1[i].length() + " " + jsonStr.length());
			//assertEquals("Line: " + i, Data_AmsPo.PO_LINES_1[i++], jsonStr);
			for (int j = 0; j < Data_DTAR.DTAR020_FIELDS_1[i].length; j++) {
				assertEquals("Line: " + i, Data_DTAR.DTAR020_FIELDS_1[i][j], l.getFieldValue(0, j).asString());
			}
			i += 1;
		}
	}

	
	private String stripSR(String s) {
		StringBuilder b = new StringBuilder(s.length());
		for (int i = 0; i < s.length(); i++) {
			if (s.charAt(i) != '\r') {
				b.append(s.charAt(i));
			}
		}
		return b.toString();
	}
//
//	private ICobol2Json getAmsC2J() {
//		return Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl"))
//		  .setFileOrganization(Constants.IO_BIN_TEXT)
//		  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
//		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
//		  
//			 .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
//			 .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
//			 .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"));
////		  .JsonArrayToCobolFile("G:/Temp/amsPoDownload_records.json", "G:/Temp/amsPoDownload_records_json.txt");
//
//	}
}
