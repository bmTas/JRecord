package net.sf.JRecord.fieldNameConversion;

/**
 * Convert a Schema (Cobol Field Name) into the java field name
 * or for the Cobol2Xml, convert Cobol to the Xml tag name
 * @author Bruce Martin
 *
 */
public interface IRenameField {
	
	/**
	 * Convert a schema (Cobol ??) to a Java name.
	 * @param schemaFieldName name of the field in the schema (Cobol Copybook)
	 * @return adjusted field-name
	 */
	public String toFieldName(String schemaFieldName);
}
