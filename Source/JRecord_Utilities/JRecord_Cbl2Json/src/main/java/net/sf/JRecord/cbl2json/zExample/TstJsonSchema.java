package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.impl.Cobol2JsonSchema;

public class TstJsonSchema {

	public static void main(String[] args) throws IOException {
		Cobol2JsonSchema.newCobol2Json(Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl"))
		  .setFileOrganization(Constants.IO_BIN_TEXT)
		  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  
			 .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
			 .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
			 .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"))
		  .cobol2jsonSchema("G:\\Temp\\aamsPoDownload.def.json");

	}

}
