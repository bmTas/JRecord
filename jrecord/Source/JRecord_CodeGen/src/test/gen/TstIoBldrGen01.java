package test.gen;

import net.sf.JRecord.cg.Generate;
import net.sf.JRecord.cg.details.ArgNames;

public class TstIoBldrGen01 {

	public static void main(String[] args) {

		String[] arguments1 = {
				ArgNames.OPT_TEMPLATE, "ioBuilder",
				ArgNames.OPT_SCHEMA, TstIoBldrGen01.class.getResource("DTAR020.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixerdWidth",
				ArgNames.OPT_FONT_NAME, "CP037",
				ArgNames.OPT_DROP_COPYBOOK_NAME, "true",
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/ioBuilder"
		};
		
		Generate.main(arguments1);

		String[] arguments2 = {
				ArgNames.OPT_TEMPLATE, "ioBuilder",
				ArgNames.OPT_SCHEMA, TstIoBldrGen01.class.getResource("ams_PO_Download.Xml").getFile(),
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/ioBuilder"
		};
		
		Generate.main(arguments2);

		
		String[] arguments3 = {
				ArgNames.OPT_TEMPLATE, "ioBuilder",
				ArgNames.OPT_SCHEMA, TstIoBldrGen01.class.getResource("MultiRecordTest.cbl").getFile(),
				ArgNames.OPT_FILE_ORGANISATION, "FixerdWidth",
				ArgNames.OPT_SPLIT, "01",
				ArgNames.OPT_OUTPUT_DIR, "G:/Temp/Gen/ioBuilder"
		};
		
		Generate.main(arguments3);
	}

}
