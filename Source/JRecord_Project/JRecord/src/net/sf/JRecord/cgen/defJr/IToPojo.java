package net.sf.JRecord.cgen.defJr;

import net.sf.JRecord.Details.AbstractLine;

public interface IToPojo<Pojo> {
	/**
	 * Convert a JRecord line to a Pojo.
	 * @param line JRecord line
	 * @return Pojo
	 */
	public Pojo toPojo(AbstractLine line);

}
