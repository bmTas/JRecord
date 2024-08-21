package net.sf.JRecord.cgen.defJr;

import net.sf.JRecord.Details.AbstractLine;

public interface IUpdateLine<Pojo> {
	
	/**
	 * This method copies the Pojo values to a JRecord line
	 * @param line JRecord line to be updated
	 * @param pojo 
	 */
	public void updateLine(AbstractLine line, Pojo pojo);

}
