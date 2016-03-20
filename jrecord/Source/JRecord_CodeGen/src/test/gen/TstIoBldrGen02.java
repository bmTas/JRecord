package test.gen;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgNames;

public class TstIoBldrGen02 {

	private static final String PACKAGE_NAME = "example.ioBuilder.nameClass";
	private static final String GENERATE_DIR = "G:/Temp/Gen/ioBuilderFN";
	private static final String IO_BUILDER_FIELD_NAME_CLASS = "ioBuilderFieldNameClass";

	public static void main(String[] args) {

		String[] arguments1 = {
				ArgNames.OPT_TEMPLATE, IO_BUILDER_FIELD_NAME_CLASS,
				ArgNames.OPT_PACKAGE, PACKAGE_NAME,
				ArgNames.OPT_SCHEMA, TstIoBldrGen02.class.getResource("DTAR020.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgNames.OPT_FONT_NAME, "CP037",
				ArgNames.OPT_DROP_COPYBOOK_NAME, "true",
				ArgNames.OPT_OUTPUT_DIR, GENERATE_DIR
		};
		Generate.main(arguments1);
	
		
		String[] arguments3 = {
				ArgNames.OPT_TEMPLATE, IO_BUILDER_FIELD_NAME_CLASS,
				ArgNames.OPT_PACKAGE, PACKAGE_NAME,
				ArgNames.OPT_SCHEMA, TstIoBldrGen02.class.getResource("MultiRecordTest.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgNames.OPT_SPLIT, "01",
				ArgNames.OPT_OUTPUT_DIR, GENERATE_DIR
		};
		
		Generate.main(arguments3);
		
		
		String[] arguments4 = {
				ArgNames.OPT_TEMPLATE, IO_BUILDER_FIELD_NAME_CLASS,
				ArgNames.OPT_PACKAGE, PACKAGE_NAME,
				ArgNames.OPT_SCHEMA, TstIoBldrGen02.class.getResource("ArrayCopybook.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixedWidth",
				ArgNames.OPT_OUTPUT_DIR, GENERATE_DIR
		};
		
		Generate.main(arguments4);

	}

}
