package net.sf.JRecord.External.base;

/**
 * This class will read a <i>child</i> Copybook file.
 * It also maintains a list of directories that must be searched for the copybook.
 * 
 * 
 * @author Bruce Martin
 *
 * @param <XRecord> JRecord-Record structure - definition (schema) of a Record in a file
 */
public interface ILoadCopybook<XRecord extends BaseExternalRecord<XRecord>> {
	/**
	 * Parse a child copybook (into the record format). This child copybook can be included 
	 * in a parent record definition. This method is called when a M<b>parse</b> tag
	 * is encountered in a RecordEditor-Xml copybnook.
	 * 
	 * @param parseDetails details from the parse tag in the parent RecordEditor-Xml.
	 * This parameter has methods to retrieve following fields<ul>
	 * <li> fileName  file to be imported
	 * <li> dbIndex current db-index
	 * <li> systemId Current System Identifier
	 * <li> language language of the Copybook
	 * <li> dialect COBOL dialect (binary representation)
	 * <li> fileStructure Current file structure
	 * <li> attributes Attributes supplied by the user
	 * </ul>
	 * 
	 * @return imported copybook
	 * @throws Exception any error that occurs
	 */
	XRecord loadCopybook(IParseDetails parseDetails) throws Exception;
	
	/**
	 * Add a directory to the copybook-directory-search list
	 * @param directoryName directory to be search for Files to be parsed
	 */
	void addSearchDirectory(String directoryName);
	
	/**
	 * remove a directory from copybook-directory-search list
	 * 
	 * @param directoryName directory to be removed from the search list
	 */
	void removeSearchDirectory(String directoryName);
}
