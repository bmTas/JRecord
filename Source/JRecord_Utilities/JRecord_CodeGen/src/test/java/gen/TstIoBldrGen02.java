package test.gen;

import java.io.IOException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstIoBldrGen02 {

	private static final String PACKAGE_NAME = "example.ioBuilder.nameClass";
	private static final String GENERATE_DIR = TstCommon.OUTPUT_DIRECTORY + "ioBuilderFN";
	private static final String IO_BUILDER_FIELD_NAME_CLASS = "standard";

	public static void main(String[] args) throws IOException,  XMLStreamException, FactoryConfigurationError {

		String[] arguments1 = {
				ArgumentOption.OPT_TEMPLATE, IO_BUILDER_FIELD_NAME_CLASS,
				ArgumentOption.OPT_PACKAGE, PACKAGE_NAME,
				ArgumentOption.OPT_SCHEMA, TstIoBldrGen02.class.getResource("DTAR020.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_FONT_NAME, "CP037",
				ArgumentOption.OPT_DROP_COPYBOOK_NAME, "true",
				ArgumentOption.OPT_OUTPUT_DIR, GENERATE_DIR
		};
		Generate.main(arguments1);
	
		
		String[] arguments3 = {
				ArgumentOption.OPT_TEMPLATE, IO_BUILDER_FIELD_NAME_CLASS,
				ArgumentOption.OPT_PACKAGE, PACKAGE_NAME,
				ArgumentOption.OPT_SCHEMA, TstIoBldrGen02.class.getResource("MultiRecordTest.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_SPLIT, "01",
				ArgumentOption.OPT_OUTPUT_DIR, GENERATE_DIR
		};
		
		Generate.main(arguments3);
		
		
		String[] arguments4 = {
				ArgumentOption.OPT_TEMPLATE, IO_BUILDER_FIELD_NAME_CLASS,
				ArgumentOption.OPT_PACKAGE, PACKAGE_NAME,
				ArgumentOption.OPT_SCHEMA, TstIoBldrGen02.class.getResource("ArrayCopybook.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_OUTPUT_DIR, GENERATE_DIR
		};
		
		Generate.main(arguments4);

	}

}
