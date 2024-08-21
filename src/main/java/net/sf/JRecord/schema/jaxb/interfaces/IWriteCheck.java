package net.sf.JRecord.schema.jaxb.interfaces;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.jaxb.IItem;

/**
 * Class to check if a Group/Field should be written
 * @author Bruce Martin
 *
 */
public interface IWriteCheck {
	/**
	 * Check wether this Item and child Items should be written
	 * @param item item being checked
	 * @param line line currently being printed
	 * @return wether to priint this item for this line.
	 */
	boolean isOkToWrite(IItem item, AbstractLine line);
}
