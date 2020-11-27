package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;



import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Cobol2Json;

public class TstCbl2JsonDtar020 {

	public static void main(String[] args) throws IOException {
		Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("DTAR020.cbl"))
				  .setFont("cp037")
				  .setFileOrganization(Constants.IO_FIXED_LENGTH) 
				  .setSplitCopybook(CopybookLoader.SPLIT_NONE)
				  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
					 
			.cobol2json(Cbl2JsonCode.getFullName("DTAR020_tst1.bin"), "G:/Temp/DTAR020_tst1.bin.json");		
	}
}
