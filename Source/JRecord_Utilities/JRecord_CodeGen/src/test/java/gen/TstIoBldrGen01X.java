package test.gen;

import java.io.IOException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstIoBldrGen01X {

	public static void main(String[] args) throws IOException,  XMLStreamException, FactoryConfigurationError {

		String[] arguments1 = {
				ArgumentOption.OPT_TEMPLATE, "basicX",
				ArgumentOption.OPT_TEMPLATE_DIRECTORY, "G:\\Temp\\Gen\\TemplateSource",
				ArgumentOption.OPT_SCHEMA, TstIoBldrGen01X.class.getResource("DTAR020.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_FONT_NAME, "CP037",
				ArgumentOption.OPT_DROP_COPYBOOK_NAME, "true",
				ArgumentOption.OPT_OUTPUT_DIR, TstCommon.IOBUILDER_OUTPUT_DIRECTORY +"X"
		};
		
		Generate.main(arguments1);

//		String[] arguments2 = {
//				ArgumentOption.OPT_TEMPLATE, "basic",
//				ArgumentOption.OPT_SCHEMA, TstIoBldrGen01X.class.getResource("ams_PO_Download.Xml").getFile(),
//				ArgumentOption.OPT_OUTPUT_DIR, TstCommon.IOBUILDER_OUTPUT_DIRECTORY
//		};
//		
//		Generate.main(arguments2);
//
//		
//		String[] arguments3 = {
//				ArgumentOption.OPT_TEMPLATE, "basic",
//				ArgumentOption.OPT_SCHEMA, TstIoBldrGen01X.class.getResource("MultiRecordTest.cbl").getFile(),
//				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
//				ArgumentOption.OPT_SPLIT, "01",
//				ArgumentOption.OPT_OUTPUT_DIR, "TstCommon.IOBUILDER_OUTPUT_DIRECTORY
//		};
//		
//		Generate.main(arguments3);
	}

}
