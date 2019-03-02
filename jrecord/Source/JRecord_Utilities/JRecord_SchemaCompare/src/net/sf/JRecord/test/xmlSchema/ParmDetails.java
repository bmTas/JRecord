package net.sf.JRecord.test.xmlSchema;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;

import net.sf.JRecord.test.schema.cobol.io.IParms;
import net.sf.JRecord.utilityClasses.ParseArguments;


/**
 * Parse arguments for the layout write program
 * 
 * @author Bruce Martin
 *
 */
public class ParmDetails implements IParms {
//	public static final Opts MAINFRAME_VB
//		= new Opts("Mainframe_VB", "Mainframe VB, file consists of <record-length><record-data>", Constants.IO_VB);
	

	public static final String ARG_DIRECTORY = "-directory";
	public static final String ARG_INPUT  = "-input";
	public static final String ARG_OUTPUT  = "-output";
	public static final String ARG_FONT = "-font";
	
	public static final String[] ARGS = {
		ARG_DIRECTORY, ARG_FONT,  
		ARG_OUTPUT, ARG_INPUT
	};
	

	public final String directory, font, outputFileName, inputFileName;
	public final int dialect=0, fileStructure = 0;
	public final boolean dropCopybookName = false;
	
	private boolean ok = true;
	
	public ParmDetails(boolean inputRequired, String[] args) {
		ParseArguments parser = new ParseArguments(ARGS, args);
		
//		String dropCopybookNameVal = parser.getArg(ARG_DROP_COPBOOK_NAME, "F");
		directory = parser.getArg(ARG_DIRECTORY, "");
		font = parser.getArg(ARG_FONT, "");
		outputFileName = parser.getArg(ARG_OUTPUT, "");
		inputFileName  = parser.getArg(ARG_INPUT, "");
		

		
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


}
