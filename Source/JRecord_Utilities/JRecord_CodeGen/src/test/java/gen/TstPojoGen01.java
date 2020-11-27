package test.gen;

import java.io.IOException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstPojoGen01 {

	public static void main(String[] args) throws IOException, XMLStreamException, FactoryConfigurationError {

		String[] arguments1 = {
				ArgumentOption.OPT_TEMPLATE, "javaPojo",
				ArgumentOption.OPT_PACKAGE, "example.pojo",
				ArgumentOption.OPT_SCHEMA, TstPojoGen01.class.getResource("DTAR020.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_FONT_NAME, "CP037",
				ArgumentOption.OPT_DROP_COPYBOOK_NAME, "true",
				ArgumentOption.OPT_OUTPUT_DIR, "G:/Temp/Gen/Pojo"
		};
		
		Generate.main(arguments1);
/*
		String[] arguments2 = {
				ArgNames.OPT_TEMPLATE, "ioBuilderWithSchemaClass",
				ArgNames.OPT_PACKAGE, "example.ioBuilderWithSchema",
				ArgNames.OPT_SCHEMA, TstBeanGen01.class.getResource("ams_PO_Download.Xml").getFile(),
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/ioBuilderSchema"
		};
		
		Generate.main(arguments2);

		
		String[] arguments3 = {
				ArgNames.OPT_TEMPLATE, "ioBuilderWithSchemaClass",
				ArgNames.OPT_PACKAGE, "example.ioBuilderWithSchema",
				ArgNames.OPT_SCHEMA, TstBeanGen01.class.getResource("MultiRecordTest.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgNames.OPT_SPLIT, "01",
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/ioBuilderSchema"
		};
				
		Generate.main(arguments3);
*/
		String[] arguments4 = {
				ArgumentOption.OPT_TEMPLATE, "javaPojo",
				ArgumentOption.OPT_PACKAGE, "example.pojo",
				ArgumentOption.OPT_SCHEMA, TstPojoGen01.class.getResource("ArrayCopybook.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgumentOption.OPT_OUTPUT_DIR, "G:/Temp/Gen/Pojo"
		};
		
		Generate.main(arguments4);

	}

}
