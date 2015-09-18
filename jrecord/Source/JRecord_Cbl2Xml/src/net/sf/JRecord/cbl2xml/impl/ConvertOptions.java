package net.sf.JRecord.cbl2xml.impl;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.utilityClasses.ParseArguments;

public class ConvertOptions {
	private static final String[] VAID_ARGS = {
		"-cobol", "-cb2xml", "-input", "-output", "-font", "-mainXmlTag",
		"-fileOrganisation", "-dialect", "-dropCopybookName",
		"-h", "-help", "-?"
	};
	private static final Opts[] FILE_ORGANISATION_OPTS = {
		new Opts("Text",         "Standard Windows/Unix text file (single byte characterset)", Constants.IO_BIN_TEXT),
		new Opts("FixedWidth",   "File where lines (records) are the same length no \\n", Constants.IO_FIXED_LENGTH),
		new Opts("Mainframe_VB", "Mainframe VB, file consists of <record-length><record-data>", Constants.IO_VB)};
	private static final Opts[] DIALECT_OPTS = {
		new Opts("Mainframe",    "Mainframe cobol",  ICopybookDialects.FMT_MAINFRAME),
		new Opts("Futjitsu",     "Fujitsu PC cobol", ICopybookDialects.FMT_FUJITSU),
		new Opts("GNUCobol",     "GNU Cobol (little endian, ie intel)", ICopybookDialects.FMT_OPEN_COBOL),
		new Opts("GNUCobolBE",   "GNU Cobol (big endian, ie IBM, Sun(oracle))", ICopybookDialects.FMT_OPEN_COBOL_BE),
	};

	
	public final String cobolCopybook, cb2xmlCopybook, inputFile, outputFile, font, mainXmlTag;
	public final int fileOrganisation, dialect;
	public final boolean dropCopybookName, useCobol;
	
	private boolean ok = true;
	
	public ConvertOptions(String[] args) {
		ParseArguments pArgs = new ParseArguments(VAID_ARGS, args);
		
		cobolCopybook = pArgs.getArg("-cobol", "");
		cb2xmlCopybook = pArgs.getArg("-cb2xml", "");
		
		inputFile  = checkRequired("input",  pArgs.getArg("-input",  ""));
		outputFile = checkRequired("output", pArgs.getArg("-output", ""));
		font = pArgs.getArg("-font", "");
		mainXmlTag = pArgs.getArg("-mainXmlTag", ICobol2Xml.MAIN_XML_TAG);
		
		fileOrganisation = decodeAsOpt(pArgs, "-fileOrganisation", false, FILE_ORGANISATION_OPTS[0], FILE_ORGANISATION_OPTS).id;
		dialect = decodeAsOpt(pArgs, "-dialect", false, DIALECT_OPTS[0], DIALECT_OPTS).id;

		dropCopybookName = pArgs.getArg("-font", "").toLowerCase().startsWith("y");
		
		boolean uc = false;
		if (pArgs.get2Args("-h", "-help", pArgs.getArg("-?")) != null) {
			ok = false;
			printOptions();
		} else {
			if (cobolCopybook.length() == 0 && cb2xmlCopybook.length() == 0) {
				ok = false;
				System.out.println("You must ");
			} else if (cobolCopybook.length() == 0) {
				uc = true;
				if (cb2xmlCopybook.length() > 0) {
					System.out.println("Warning both cb2xml and cobol copybooks have been provided, only one is needed, using Cobol ");
				}
			}
		}
		
		useCobol = uc;
	}
	
	private String checkRequired(String name, String value) {
		if (value.length() == 0) {
			ok = false;
			System.out.println("Parameter -" + name + " is required !!!");
		}
		return value;
	}
	
	/**
	 * @return the ok
	 */
	public final boolean isOk() {
		return ok;
	}

	private void printOptions() {
		
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
		public final String option, description;
		public final int id;
		
		private Opts(String option, String description) {
			this(option,  description, 0);
		}
		
		private Opts(String option, String description, int id) {
			super();
			this.option = option;
			this.description = description;
			this.id = id;
		}
	}

}
