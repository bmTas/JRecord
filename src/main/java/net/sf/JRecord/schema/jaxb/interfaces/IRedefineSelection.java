package net.sf.JRecord.schema.jaxb.interfaces;

import java.util.List;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.schema.jaxb.IItem;

/**
 * Class to select which redefines
 * @author Bruce Martin
 *
 */
public interface IRedefineSelection {
	/**
	 * Select the Cobol Group's/Fields to be written in a from a list of redefined groups
	 * @param redefinedItemGroup list of Cobol-items that have been redefined
	 * @return Items that are to be written
	 */
	List<IItem> selectRedefinedItemToWrite(List<IItem> redefinedItemGroup, AbstractLine line);
}
