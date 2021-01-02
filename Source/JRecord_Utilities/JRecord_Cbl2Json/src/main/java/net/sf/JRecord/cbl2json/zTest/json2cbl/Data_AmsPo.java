package net.sf.JRecord.cbl2json.zTest.json2cbl;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class Data_AmsPo {

	public static final String[] PO_LINES_1 = {
//			"H1453500000006228000000222227040909        00  200 0501020501075966        LADIES KNICFT",
//			"D100108000000000014832000000000000000 62225071000000054540000108       2075350        65615071       M.ROSE 24-006607 SHWL WRAP CARD",                   
			"H1453500000006228000000222227040909        00  200 050102050107            LADIES KNICFT",
			"D100108000000000014832000000000000000 62225071                         2075350        65615071       M.ROSE 24-006607 SHWL WRAP CARD                   ",
			"S1501500000002501900000005503300000002503500000002503700000004505200000002505500000002506000000002507000000002507400000004",
	};
	public static final String[] PO_JSON_LINES_1 = {
			      "{\n"
				+ "  \"PO_Record\" : {\n"
				+ "    \"Record_Type\" : \"H1\",\n"
				+ "    \"Sequence_Number\" : 45.350,\n"
				+ "    \"Vendor\" : 6228,\n"
				+ "    \"PO\" : 222227,\n"
				+ "    \"Entry_Date\" : \"040909\",\n"
				+ "    \"beg01_code\" : \"00\",\n"
				+ "    \"Department\" : \"200\",\n"
				+ "    \"Expected_Reciept_Date\" : \"050102\",\n"
				+ "    \"Cancel_by_date\" : \"050107\",\n"
				+ "    \"Department_Name\" : \"LADIES KNI\",\n"
				+ "    \"Prcoess_Type\" : \"C\",\n"
				+ "    \"Order_Type\" : \"FT\"\n"
				+ "  }\n"
				+ "}"

				,
						
				  "{\n"
				+ "  \"Product_Record\" : {\n"
				+ "    \"Record_Type\" : \"D1\",\n"
				+ "    \"Pack_Qty\" : 108.0000,\n"
				+ "    \"Pack_Cost\" : 148.3200,\n"
				+ "    \"APN\" : 0,\n"
				+ "    \"Product\" : 62225071,\n"
				+ "    \"pmg_dtl_tech_key\" : \"2075350\",\n"
				+ "    \"Case_Pack_id\" : \"65615071\",\n"
				+ "    \"Product_Name\" : \"M.ROSE 24-006607 SHWL WRAP CARD\"\n"
				+ "  }\n"
				+ "}"

				,
				
				  "{\n"
				+ "  \"Location_Record\" : {\n"
				+ "    \"Record_Type\" : \"S1\",\n"
				+ "    \"location\" : [ {\n"
				+ "      \"DC_Number\" : 5015,\n"
				+ "      \"Pack_Quantity\" : 2\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5019,\n"
				+ "      \"Pack_Quantity\" : 5\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5033,\n"
				+ "      \"Pack_Quantity\" : 2\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5035,\n"
				+ "      \"Pack_Quantity\" : 2\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5037,\n"
				+ "      \"Pack_Quantity\" : 4\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5052,\n"
				+ "      \"Pack_Quantity\" : 2\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5055,\n"
				+ "      \"Pack_Quantity\" : 2\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5060,\n"
				+ "      \"Pack_Quantity\" : 2\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5070,\n"
				+ "      \"Pack_Quantity\" : 2\n"
				+ "    }, {\n"
				+ "      \"DC_Number\" : 5074,\n"
				+ "      \"Pack_Quantity\" : 4\n"
				+ "    } ]\n"
				+ "  }\n"
				+ "}"

	};

	public static String toJsonStr() {
		return toJsonStr(PO_JSON_LINES_1);
	}
	
	public static String toJsonStr(String[] jsonLines) {
	String sep = "";
		StringBuilder b = new StringBuilder()
				.append("{\n   \"Lines\": [");
		
		for (String s : jsonLines) {
			b.append(sep).append(s);
			sep = ",\n";
		}
		
		return b.append("]\n}").toString();
	}
	

	public static ICobol2Json getAmsC2J() {
		return Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl"))
		  .setFileOrganization(Constants.IO_BIN_TEXT)
		  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  
			 .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
			 .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
			 .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"));
//		  .JsonArrayToCobolFile("G:/Temp/amsPoDownload_records.json", "G:/Temp/amsPoDownload_records_json.txt");

	}

}
