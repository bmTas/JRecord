package net.sf.JRecord.cg.details;

import net.sf.JRecord.utilityClasses.ParseArguments;

public class ParseArgs extends ParseArguments implements ArgNames {
	
	private static final String[] SINGLE_ARGS = {
		OPT_SCHEMA, ArgNames.OPT_PACKAGE, OPT_TEMPLATE, 
		OPT_FILE_ORGANISATION,
		OPT_LOAD_SCHEMA, "-packageId", "-h", "-?", "-help", OPT_SPLIT,
		OPT_FONT_NAME, OPT_DROP_COPYBOOK_NAME, OPT_OUTPUT_DIR
	};
	private static final String[] MULTI_ARGS = {
		OPT_GENERATE
	};
	
	public ParseArgs(String... args) {
		super(SINGLE_ARGS, MULTI_ARGS, args);
	}
}
