package net.sf.JRecord.test.schema;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.test.schema.cobol.io.IParms;
import net.sf.JRecord.utilityClasses.ParseArguments;


/**
 * Parse arguments for the layout write program
 * 
 * @author Bruce Martin
 *
 */
public class ParmDetails implements IParms {
	public static final Opts MAINFRAME_VB
		= new Opts("Mainframe_VB", "Mainframe VB, file consists of <record-length><record-data>", Constants.IO_VB);
	
	public static final String ARG_DIALECT = "-dialect";
	public static final String ARG_DIRECTORY = "-directory";
	public static final String ARG_INPUT  = "-input";
	public static final String ARG_OUTPUT  = "-output";
	public static final String ARG_FONT = "-font";
	public static final String ARG_FILE_STRUCTURE = "-fileStructure";
	public static final String ARG_DROP_COPBOOK_NAME = "-DropCopybookName";
	
	public static final String[] ARGS = {
		ARG_DIALECT, ARG_DIRECTORY, ARG_FONT, ARG_DROP_COPBOOK_NAME, ARG_FILE_STRUCTURE,
		ARG_OUTPUT, ARG_INPUT
	};
	
	private static final Opts[] DIALECT_OPTS = {
		new Opts("Mainframe",    "Mainframe cobol",  ICopybookDialects.FMT_MAINFRAME),
		new Opts("Futjitsu",     "Fujitsu PC cobol", ICopybookDialects.FMT_FUJITSU),
		new Opts("GNUCobol",     "GNU Cobol (little endian, ie intel)",         ICopybookDialects.FMT_GNU_COBOL),
		new Opts("GNUCobolBE",   "GNU Cobol (big endian, ie IBM, Sun(oracle))", ICopybookDialects.FMT_GNU_COBOL_BE),
	};

	private static final Opts[] FILE_ORGANISATION_OPTS = {
		new Opts("Default", "    ", "JRecord choose based on Copybook", Constants.IO_DEFAULT),
		new Opts("Text", "    ", "Standard Windows/Unix text file (single byte characterset)", Constants.IO_BIN_TEXT),
		new Opts("UnicodeText", "    ", "Standard Windows/Unix Unicode text file", Constants.IO_UNICODE_TEXT),
		new Opts("FixedWidth",   "File where lines (records) are the same length no \\n", Constants.IO_FIXED_LENGTH),
		new Opts("FixedText", "    ", "Fixed width Text (possibly unicode)", Constants.IO_FIXED_LENGTH_CHAR),
		MAINFRAME_VB,
		new Opts("GNUCobol_VB",  "GNU Cobol VB, file consists of <record-length><record-data>", Constants.IO_VB_GNU_COBOL)
	};

	public final String directory, font, outputFileName, inputFileName;
	public final int dialect, fileStructure;
	public final boolean dropCopybookName;
	
	private boolean ok = true;
	
	public ParmDetails(boolean inputRequired, String[] args) {
		ParseArguments parser = new ParseArguments(ARGS, args);
		
		String dropCopybookNameVal = parser.getArg(ARG_DROP_COPBOOK_NAME, "F");
		directory = parser.getArg(ARG_DIRECTORY, "");
		font = parser.getArg(ARG_FONT, "");
		outputFileName = parser.getArg(ARG_OUTPUT, "");
		inputFileName  = parser.getArg(ARG_INPUT, "");
		
		dropCopybookName = "yes".equalsIgnoreCase(dropCopybookNameVal)
				|| "y".equalsIgnoreCase(dropCopybookNameVal)
				|| "true".equalsIgnoreCase(dropCopybookNameVal);
		
		dialect = decodeAsOpt(parser, ARG_DIALECT, false, DIALECT_OPTS[0], DIALECT_OPTS).id;
		fileStructure = decodeAsOpt(parser, ARG_FILE_STRUCTURE, false, MAINFRAME_VB, FILE_ORGANISATION_OPTS).id;
		
		if (directory.length() == 0) {
			System.out.println("You must supply a directory");
			ok = false;
		}
		
		if (inputRequired) {
			if (inputFileName.length() == 0) {
				System.out.println("You must supply an input file name");
				ok = false;
			}
		} else if (outputFileName.length() == 0) {
			System.out.println("You must supply an output file name");
			ok = false;
		}
	}
	
//	
//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.test.schema.IParms#getDirectory()
//	 */
//	@Override
//	public final String getDirectory() {
//		return directory;
//	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.test.schema.IParms#getFont()
	 */
	@Override
	public final String getFont() {
		return font;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.test.schema.IParms#getOutputFileName()
	 */
	@Override
	public final String getOutputFileName() {
		return outputFileName;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.test.schema.IParms#getInputFileName()
	 */
	@Override
	public final String getInputFileName() {
		return inputFileName;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.test.schema.IParms#getDialect()
	 */
	@Override
	public final int getDialect() {
		return dialect;
	}


	/**
	 * @return the fileStructure
	 */
	@Override
	public final int getFileStructure() {
		return fileStructure;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.test.schema.IParms#isDropCopybookName()
	 */
	@Override
	public final boolean isDropCopybookName() {
		return dropCopybookName;
	}

	@Override
	public String[] getFileNames() {
		File[] list = (new File(directory)).listFiles();
		ArrayList<String> fileNames = new ArrayList<String>(list.length);
		
		for (File f : list) {
			if (f.isFile()) {
				fileNames.add(f.getPath());
			}
		}
		
		Collections.sort(fileNames, String.CASE_INSENSITIVE_ORDER);
		
		return fileNames.toArray(new String[fileNames.size()]);
	}

	/**
	 * @return the ok
	 */
	public final boolean isOk() {
		return ok;
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
