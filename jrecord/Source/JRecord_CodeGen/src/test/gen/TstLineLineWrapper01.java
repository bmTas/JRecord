package test.gen;

import java.io.IOException;

import javax.xml.bind.JAXBException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstLineLineWrapper01 {

	public static void main(String[] args) throws IOException, JAXBException {

//		String[] arguments1 = {
//				ArgNames.OPT_TEMPLATE, "lineWrapper",
//				ArgNames.OPT_PACKAGE, "dtar020.lineWrapper",
//				ArgNames.OPT_SCHEMA, TstLineLineWrapper01.class.getResource("DTAR020.cbl").getFile(),
//				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
//				ArgNames.OPT_FONT_NAME, "CP037",
////				ArgNames.OPT_SPLIT, "01",
//				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/lineWrapper"
//		};
//		
//		Generate.main(arguments1);
		
		String[] arguments2 = {
				ArgumentOption.OPT_TEMPLATE, "lineWrapper",
				ArgumentOption.OPT_PACKAGE, "amsPoDownload.lineWrapper",
				ArgumentOption.OPT_SCHEMA, TstLineLineWrapper01.class.getResource("ams_Po_Download.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_SPLIT, "01",
				ArgumentOption.OPT_OUTPUT_DIR, "G:/Temp/Gen/lineWrapper"
		};
		
		Generate.main(arguments2);

	}

}
