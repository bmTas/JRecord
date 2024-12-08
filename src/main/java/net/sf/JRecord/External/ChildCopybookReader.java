package net.sf.JRecord.External;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.External.base.ILoadCopybook;
import net.sf.JRecord.External.base.IParseDetails;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.constantNames.ConstantNameConversion;
import net.sf.JRecord.constantNames.ConstantNames;
import net.sf.JRecord.constantNames.ConstantNames.ConstantConversions;

/**
 * This class will load A `CHILD` COBOL/Xml copybook from a file and return it to the calling program.
 * It is called from the class {@link net.sf.JRecord.External.base.BaseRecordEditorXmlLoader} when a
 * parse xml-tag is found
 * 
 * @author Bruce Martin
 *
 */
public class ChildCopybookReader implements ILoadCopybook<ExternalRecord> {

	private static final String RECORD_EDITOR_XML = "RecordEditorXml";
	private static final String COBOL = "Cobol";
	
//	private final ExternalRecordBuilder recordBuilder = new ExternalRecordBuilder();
	private final AbsSSLogger log;
	
	private final List<String> searchDirectories;
	private final int defaultCobolDialect;
//	private List<String> searchDirectories = new ArrayList<>();
	
	
	public ChildCopybookReader(List<String> dirs, String localDirectory, int defaultCobolDialect, AbsSSLogger log) {
		super();
		this.log = log;
		this.defaultCobolDialect = defaultCobolDialect;
		if (localDirectory == null) {
			this.searchDirectories = dirs == null ? new ArrayList<String>() : new ArrayList<String>(dirs);
		} else {
			this.searchDirectories = new ArrayList<>(dirs);
			this.searchDirectories.add(localDirectory);
			this.searchDirectories.addAll(dirs);
		}
	}



	@Override
	public void addSearchDirectory(String directoryName) {
		searchDirectories.remove(directoryName);
		searchDirectories.add(directoryName);
	}



	@Override
	public void removeSearchDirectory(String directoryName) {
		searchDirectories.remove(directoryName);
	}



	@Override
	public ExternalRecord loadCopybook(IParseDetails parseDetails) throws Exception {
		
		String fileName = parseDetails.getFileName();
		String language = parseDetails.getLanguage();
		if (fileName == null || fileName.length() == 0) {
			throw new RuntimeException("You must specify a filename on an import Xml Tag");
		}
		Path childFile=null, testPath = Paths.get(fileName);
		
		if (Files.exists(testPath)) {
			childFile = testPath;
		} else {
			if (searchDirectories != null) {
				for (String d : searchDirectories) {
					childFile = Paths.get(d).resolve(fileName);
					if (Files.exists(childFile)) {
						break;
					}
				}
			}
			if (childFile == null) {
				throw new RuntimeException("Could not find " + fileName);
			}
		}
		
		
		if (language == null || language.length() == 0) {
			String lcFileName = fileName.toLowerCase();
			if (lcFileName.endsWith(".cbl") || lcFileName.endsWith(".cobol") || lcFileName.endsWith(".cob")) {
				language = COBOL;
			} else if (lcFileName.endsWith("xml")) {
				language = RECORD_EDITOR_XML;
			}
		}
		
		String fullFileName = childFile.toString();
		String fontname = parseDetails.getFontname();
		int systemIdentifier = parseDetails.getSystemIdentifier();
		
		if (COBOL.equalsIgnoreCase(language)) {
			ConstantConversions constantConversions = ConstantNames.getConstantConversions();
			int split = lookupCode(constantConversions.getSplitOptions(), 
					parseDetails.getAttribute("split"),
					ICobolSplitOptions.SPLIT_NONE);
			int dialect = lookupCode(constantConversions.getDialects(), 
					parseDetails.getAttribute("dialect"),
					defaultCobolDialect);
			int copybookFormat = lookupCode(constantConversions.getCopybookFormat(),
					parseDetails.getAttribute("copybookFormat"),
					ICopybookDialects.FMT_MAINFRAME);
			
			return new CobolCopybookLoader().loadCopyBook(
					fullFileName, split, parseDetails.getDbIdentifier(), fontname,
					copybookFormat, dialect, systemIdentifier, log);
		} else if ("Xml".equalsIgnoreCase(language) || RECORD_EDITOR_XML.equalsIgnoreCase(language)) {
			return new RecordEditorXmlLoader().loadCopyBook(
					fullFileName, 0, parseDetails.getDbIdentifier(), fontname,
					parseDetails.getCobolDialect(), systemIdentifier, log);
		}
		throw new RuntimeException("Invalid language: " + language
				+ " expecting one of: " + COBOL + ", " + RECORD_EDITOR_XML);
	}

	/**
	 * Convert a String to the equivalent JRecord code
	 * 
	 * @param converion String-to-integer-conversion class
	 * @param key String key to lookup
	 * @param defaultValue default value to be used if the code is not found
	 * @return requested code
	 */
	private int lookupCode(ConstantNameConversion converion, String key, int defaultValue) {
		int ret = defaultValue;
		if (key != null && key.length() > 0) {
			ret = converion.getCode(key);
			if (ret < 0) {
				ret = defaultValue;
			}
		}
		return ret;
	}
	
}
