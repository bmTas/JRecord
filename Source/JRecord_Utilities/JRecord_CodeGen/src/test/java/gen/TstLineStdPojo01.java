package test.gen;

import java.io.IOException;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgumentOption;

public class TstLineStdPojo01 {

	public static void main(String[] args) throws IOException, XMLStreamException, FactoryConfigurationError {

		String[] arguments1 = {
				ArgumentOption.OPT_TEMPLATE, "stdPojo",
				ArgumentOption.OPT_PACKAGE, "stdPojo.gen",
				ArgumentOption.OPT_SCHEMA, TstLineStdPojo01.class.getResource("aqtrans.cbl").getFile(),
				ArgumentOption.OPT_FILE_ORGANISATION, "Mainframe_VB",
				ArgumentOption.OPT_FONT_NAME, "CP037",
				ArgumentOption.OPT_SPLIT, "01",
				ArgumentOption.OPT_OUTPUT_DIR, "G:/Temp/Gen/stdPojo"
		};
		
		Generate.main(arguments1);
//		Generate.main(new String[0]);
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
/*		String[] arguments4 = {
				ArgNames.OPT_TEMPLATE, "javaPojo",
				ArgNames.OPT_PACKAGE, "example.pojo",
				ArgNames.OPT_SCHEMA, TstLineStdGen01.class.getResource("ArrayCopybook.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/Pojo"
		};
		
		Generate.main(arguments4);
*/
	}

}
