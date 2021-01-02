/**
 * 
 */
package net.sf.cobolToJson.impl.readJson;

import net.sf.JRecord.Details.AbstractLine;

/**
 * @author Bruce01
 *
 */
public abstract class AFieldsToJRecLine implements IProcessFields {

	protected abstract AbstractLine getLine();

	/* (non-Javadoc)
	 * @see net.sf.cobolToJson.impl.readJson.IProcessFields#updateField(java.lang.String, long)
	 */
	@Override
	public void updateField(String fieldName, long value) {
		getLine().getFieldValue(fieldName).set(value);
	}

	/* (non-Javadoc)
	 * @see net.sf.cobolToJson.impl.readJson.IProcessFields#updateField(java.lang.String, java.lang.String)
	 */
	@Override
	public void updateField(String fieldName, String value) {
		getLine().getFieldValue(fieldName).set(value);
	}

}
