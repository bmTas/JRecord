package net.sf.JRecord.cbl2xml.impl;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.utilityClasses.ParseArguments;
 
public class ConvertOptions {
	private static final String OPT_DROP_COPYBOOK_NAME = "-dropCopybookName";
	private static final String OPT_DIALECT = "-dialect";
	private static final String OPT_FILE_ORGANISATION = "-fileOrganisation";
	private static final String OPT_MAIN_XML_TAG = "-mainXmlTag";
	private static final String OPT_FONT = "-font";
	private static final String OPT_OUTPUT = "-output";
	private static final String OPT_INPUT = "-input";
	private static final String OPT_CB2XML = "-cb2xml";
	private static final String OPT_COBOL  = "-cobol";
	private static final String[] VAID_ARGS = {
		OPT_COBOL, OPT_CB2XML, OPT_INPUT, OPT_OUTPUT, OPT_FONT, OPT_MAIN_XML_TAG,
		OPT_FILE_ORGANISATION, OPT_DIALECT, OPT_DROP_COPYBOOK_NAME,
		"-h", "-help", "-?"
	};
	private static final Opts[] FILE_ORGANISATION_OPTS = {
		new Opts("Text", "    ", "Standard Windows/Unix text file (single byte characterset)", Constants.IO_BIN_TEXT),
		new Opts("FixedWidth",   "File where lines (records) are the same length no \\n", Constants.IO_FIXED_LENGTH),
		new Opts("Mainframe_VB", "Mainframe VB, file consists of <record-length><record-data>", Constants.IO_VB),
		new Opts("GNUCobol_VB",  "GNU Cobol VB, file consists of <record-length><record-data>", Constants.IO_VB_OPEN_COBOL)
	};
	private static final Opts[] DIALECT_OPTS = {
		new Opts("Mainframe",    "Mainframe cobol",  ICopybookDialects.FMT_MAINFRAME),
		new Opts("Futjitsu",     "Fujitsu PC cobol", ICopybookDialects.FMT_FUJITSU),
		new Opts("GNUCobol",     "GNU Cobol (little endian, ie intel)",         ICopybookDialects.FMT_OPEN_COBOL),
		new Opts("GNUCobolBE",   "GNU Cobol (big endian, ie IBM, Sun(oracle))", ICopybookDialects.FMT_OPEN_COBOL_BE),
	};

	
	public final String cobolCopybook, cb2xmlCopybook, inputFile, outputFile, font, mainXmlTag;
	public final int fileOrganisation, dialect;
	public final boolean dropCopybookName, useCobol;
	
	private boolean ok = true;
	
	public ConvertOptions(String[] args) {
		ParseArguments pArgs = new ParseArguments(VAID_ARGS, args);
		
		cobolCopybook = pArgs.getArg(OPT_COBOL, "");
		cb2xmlCopybook = pArgs.getArg(OPT_CB2XML, "");
		
		inputFile  = pArgs.getArg(OPT_INPUT,  "");
		outputFile = pArgs.getArg(OPT_OUTPUT, "");
		font = pArgs.getArg(OPT_FONT, "");
		mainXmlTag = pArgs.getArg(OPT_MAIN_XML_TAG, ICobol2Xml.MAIN_XML_TAG);
		
		fileOrganisation = decodeAsOpt(pArgs, OPT_FILE_ORGANISATION, false, FILE_ORGANISATION_OPTS[0], FILE_ORGANISATION_OPTS).id;
		dialect = decodeAsOpt(pArgs, OPT_DIALECT, false, DIALECT_OPTS[0], DIALECT_OPTS).id;

		String drop = pArgs.getArg(OPT_DROP_COPYBOOK_NAME, "").toLowerCase();
		dropCopybookName = drop.startsWith("t") || drop.startsWith("y");

		
		boolean uc = false;
		if (args.length == 0 || pArgs.get2Args("-h", "-help", pArgs.getArg("-?")) != null) {
			ok = false;
			printOptions();
		} else {
			checkRequired(OPT_INPUT, inputFile);
			checkRequired(OPT_OUTPUT, outputFile);
			if (cobolCopybook.length() == 0 && cb2xmlCopybook.length() == 0) {
				ok = false;
				System.out.println("You must ");
			} else if (cobolCopybook.length() > 0) {
				uc = true;
				if (cb2xmlCopybook.length() > 0) {
					System.out.println("Warning both cb2xml and cobol copybooks have been provided, only one is needed, using Cobol ");
				}
			}
		}
		
		useCobol = uc;
	}
	
	private void checkRequired(String name, String value) {
		if (value.length() == 0) {
			ok = false;
			System.out.println("Parameter -" + name + " is required !!!");
		}
	}
	
	/**
	 * @return the ok
	 */
	public final boolean isOk() {
		return ok;
	}

	private void printOptions() {
		System.out.println();
		System.out.println("  Program options:");
		System.out.println();
		System.out.println("          " + OPT_COBOL  + "\t- Cobol  copybook used to \"interpret\" the data (you must supply either a cobol or cb2xml copybook");
		System.out.println("          " + OPT_CB2XML + "\t- Cb2xml copybook used to \"interpret\" the data");
		System.out.println();
		System.out.println("          " + OPT_INPUT  + "\t- Input file");
		System.out.println("          " + OPT_OUTPUT + "\t- Output file");
		System.out.println("          " + OPT_FONT   + "  \t- Characterset used in the Cobol data file (e.g. IBM037 for US-EBCIDIC)");
		System.out.println();
		System.out.println("          " + OPT_DROP_COPYBOOK_NAME + "\t- (true/false) wether to drop the cobol copybook name from the start of the Xml Tags");
		System.out.println();
		System.out.println("          " + OPT_MAIN_XML_TAG + "     \t- The outermost Xml tag to use (default coboldata)");
		System.out.println();
		System.out.println("          " + OPT_FILE_ORGANISATION + "\t- \"file organisation\" of the Cobol data file");
		printList(FILE_ORGANISATION_OPTS);

		System.out.println();
		System.out.println("          " + OPT_DIALECT  + "\t- Cobol Dialect");
		printList(DIALECT_OPTS);

		System.out.println();
	}

	
	private static void printList(Opts[] list) {
		for (Opts o : list) {
			System.out.println("\t\t" + o.option + o.pad + "\t- " + o.description );
		}
	}

	private Opts decodeAsOpt(ParseArguments pa, String key, boolean printMsg, Opts defaultOption, Opts[] opts) {
		return decodeAsOpt(pa.getArg(key), key, printMsg, defaultOption, opts);
	}

	private Opts decodeAsOpt(String v, String key, boolean printMsg, Opts defaultOption, Opts[] opts) {
		
		if (v != null) {
			for (Opts o : opts) {
				if (o.option.equalsIgnoreCase(v)) {
					
					return o;
				}
			}
			if (printMsg) {
				System.out.println("Invalid value " + v + " for argument " + key);
				ok = false;
			}
		}
		
		return defaultOption;
	}


	public static final class Opts {
		public final String option, pad, description;
		public final int id;
		
		private Opts(String option, String description, int id) {
			this(option, "", description, id);
		}

		private Opts(String option, String pad, String description, int id) {
			super();
			this.option = option;
			this.pad = pad;
			this.description = description;
			this.id = id;
		}
	}

}
