package test.gen;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgNames;

public class TstPojoGen01 {

	public static void main(String[] args) {

		String[] arguments1 = {
				ArgNames.OPT_TEMPLATE, "javaPojo",
				ArgNames.OPT_PACKAGE, "example.pojo",
				ArgNames.OPT_SCHEMA, TstPojoGen01.class.getResource("DTAR020.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgNames.OPT_FONT_NAME, "CP037",
				ArgNames.OPT_DROP_COPYBOOK_NAME, "true",
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/Pojo"
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
				ArgNames.OPT_TEMPLATE, "javaPojo",
				ArgNames.OPT_PACKAGE, "example.pojo",
				ArgNames.OPT_SCHEMA, TstPojoGen01.class.getResource("ArrayCopybook.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/Pojo"
		};
		
		Generate.main(arguments4);

	}

}
