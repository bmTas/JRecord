package net.sf.JRecord.schema.jaxb.interfaces;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.schema.jaxb.IItem;

/**
 * Class to format a field before witing it out
 * @author Bruce Martin
 *
 */
public interface IFormatField {
	/**
	 * Format the field
	 * @param itemDef Cobol Item definition. holds all the Cobol details e.g. display length etc 
	 * @param fieldDef JRecord Field definition
	 * @param value value to be formatted
	 * @return Reformatted field
	 */
	public String format(IItem itemDef, IFieldDetail fieldDef, String value);
}
