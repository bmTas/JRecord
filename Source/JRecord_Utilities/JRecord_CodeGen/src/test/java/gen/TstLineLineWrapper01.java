package test.gen;

import java.io.IOException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstLineLineWrapper01 {

	public static void main(String[] args) throws IOException, XMLStreamException, FactoryConfigurationError {

		String[] arguments1 = {
				ArgumentOption.OPT_TEMPLATE, "lineWrapper",
				ArgumentOption.OPT_PACKAGE, "dtar020.lineWrapper",
				ArgumentOption.OPT_SCHEMA, TstLineLineWrapper01.class.getResource("DTAR020.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_FONT_NAME, "CP037",
//				ArgumentOption.OPT_SPLIT, "01",
				ArgumentOption.OPT_OUTPUT_DIR, TstCommon.OUTPUT_DIRECTORY + "lineWrapper"
		};
		
		Generate.main(arguments1);
		
		String[] arguments2 = {
				ArgumentOption.OPT_TEMPLATE, "lineWrapper",
				ArgumentOption.OPT_PACKAGE, "amsPoDownload.lineWrapper",
				ArgumentOption.OPT_SCHEMA, TstLineLineWrapper01.class.getResource("ams_Po_Download.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_SPLIT, "01",
				ArgumentOption.OPT_OUTPUT_DIR, TstCommon.OUTPUT_DIRECTORY + "lineWrapper"
		};
		
		Generate.main(arguments2);

	}

}
