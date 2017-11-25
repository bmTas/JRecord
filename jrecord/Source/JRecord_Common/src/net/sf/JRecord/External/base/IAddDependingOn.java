package net.sf.JRecord.External.base;

import net.sf.JRecord.External.Def.DependingOn;


/**
 * 
 * @author Bruce Martin
 */
public interface IAddDependingOn {

	/**
	 * Add a <i>Occurs Depending on</i> details
	 * @param child occurs 
	 */
	void addDependingOn(DependingOn child);

}