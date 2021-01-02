package net.sf.JRecord.cbl2json.zTest.json2cbl;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.StringReader;
import java.util.List;

import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;

import net.sf.JRecord.Details.AbstractLine;

public class TstReadJson {

	@Test
	public void test() throws JsonParseException, IOException {
		//ICobol2Json c2j = getAmsC2J();
		int i = 0;
		System.out.println(Data_AmsPo.toJsonStr());
		List<AbstractLine> lineList = Data_AmsPo.getAmsC2J()
			.jsonArrayToCobolLines(new StringReader(Data_AmsPo.toJsonStr()));
		for (AbstractLine line : lineList) {
			assertEquals(Data_AmsPo.PO_LINES_1[i++], line.getFullLine());
		}
	}


	@Test
	public void testDTAR020() throws JsonParseException, IOException {
		//ICobol2Json c2j = getAmsC2J();
		String jsonStr = Data_AmsPo.toJsonStr(Data_DTAR.DTAR020_JSON_LINES_1);
		System.out.println(jsonStr);
		List<AbstractLine> lineList = Data_DTAR.getDTAR020C2J()
			.jsonArrayToCobolLines(new StringReader(jsonStr));
		int i = 0;
		for (AbstractLine line : lineList) {
			for (int j = 0; j < Data_DTAR.DTAR020_FIELDS_1[i].length; j++) {
				assertEquals("Line: " + i, Data_DTAR.DTAR020_FIELDS_1[i][j], line.getFieldValue(0, j).asString());
			}
			i+=1;
		}
	}

//	private ICobol2Json getAmsC2J() {
//		return Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl"))
//		  .setFileOrganization(Constants.IO_BIN_TEXT)
//		  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
//		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
//		  
//			 .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
//			 .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
//			 .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"));
//	}

}
