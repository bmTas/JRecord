package net.sf.JRecord.cbl2json.zTest.json2cbl;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class Data_DTAR {

	public static final String[][] DTAR020_FIELDS_1 = {
			{"63604808", "20", "40118", "170", "1", "4.87",},
			{"69684558", "20", "40118", "280", "1", "19.00",},
			{"69684558", "20", "40118", "280", "-1", "-19.00",},
	};

	public static final String[] DTAR020_JSON_LINES_1 = {
			  "{\n"
			+ "  \"DTAR020_KCODE_STORE_KEY\" : {\n"
			+ "    \"DTAR020_KEYCODE_NO\" : \"63604808\",\n"
			+ "    \"DTAR020_STORE_NO\" : 20\n"
			+ "  },\n"
			+ "  \"DTAR020_DATE\" : 40118,\n"
			+ "  \"DTAR020_DEPT_NO\" : 170,\n"
			+ "  \"DTAR020_QTY_SOLD\" : 1,\n"
			+ "  \"DTAR020_SALE_PRICE\" : 4.87\n"
			+ "}"
			, "{\n"
			+ "  \"DTAR020_KCODE_STORE_KEY\" : {\n"
			+ "    \"DTAR020_KEYCODE_NO\" : \"69684558\",\n"
			+ "    \"DTAR020_STORE_NO\" : 20\n"
			+ "  },\n"
			+ "  \"DTAR020_DATE\" : 40118,\n"
			+ "  \"DTAR020_DEPT_NO\" : 280,\n"
			+ "  \"DTAR020_QTY_SOLD\" : 1,\n"
			+ "  \"DTAR020_SALE_PRICE\" : 19.00\n"
			+ "}"
			, "{\n"
			+ "  \"DTAR020_KCODE_STORE_KEY\" : {\n"
			+ "    \"DTAR020_KEYCODE_NO\" : \"69684558\",\n"
			+ "    \"DTAR020_STORE_NO\" : 20\n"
			+ "  },\n"
			+ "  \"DTAR020_DATE\" : 40118,\n"
			+ "  \"DTAR020_DEPT_NO\" : 280,\n"
			+ "  \"DTAR020_QTY_SOLD\" : -1,\n"
			+ "  \"DTAR020_SALE_PRICE\" : -19.00\n"
			+ "}"
	};
	
	

	public static ICobol2Json getDTAR020C2J() {
		return Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/DTAR020.cbl"))
		  .setFileOrganization(Constants.IO_FIXED_LENGTH)
		  .setSplitCopybook(CopybookLoader.SPLIT_NONE)
		  .setFont("cp037")
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  ;
//		  .JsonArrayToCobolFile("G:/Temp/amsPoDownload_records.json", "G:/Temp/amsPoDownload_records_json.txt");

	}
}
