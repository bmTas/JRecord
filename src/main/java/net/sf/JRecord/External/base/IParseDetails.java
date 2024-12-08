package net.sf.JRecord.External.base;


/**
 * In RecordEditor-Xml you can use a parse Tag to embed a Cobol/RecordEditor-Xml copybook
 * in the current copybook (file schema).
 * <p>This interface holds the details entered on the Parse Tag plus other relevant attributes from
 * the parent RecordEditor-Xml. 
 * It is used to send details from the Parse Tag to a {@link ILoadCopybook} class to allow
 * the child copybook to be read.
 * 
 * @author Bruce Martin
 *
 */
public interface IParseDetails {
	/**
	 * @return Database id (RecordEditor)
	 */
	int getDbIdentifier();
	/**
	 * @return Cobol Dialect
	 */
	int getCobolDialect();
	/**
	 * @return System Identifier
	 */
	int getSystemIdentifier();
	/**
	 * @return File-Name of the copybook to be read
	 */
	String getFileName();
	/**
	 * @return Langauge of the Copybook (i.e. Cobol / Xml)
	 */
	String getLanguage();
	/**
	 * 
	 * @param attributeName attribute being requested
	 * @return value of the requestedf attribute
	 */
	String getAttribute(String attributeName);
	/**
	 * @return the fontname (encoding)
	 */
	String getFontname();
	
}
