package net.sf.JRecord.detailsSelection;

import java.util.List;

import net.sf.JRecord.Common.AbstractIndexedLine;

public interface IGetValue {

	/**
	 * is the value going to be numeric ???
	 * @return is the value going to be numeric ???
	 */
	public boolean isNumeric();

	/**
	 * get the value from a line
	 * @param line to extract the value from
	 * @return requested value
	 */
	public Object getValue(AbstractIndexedLine line);

	/**
	 * Wether this record should be Tested ???
	 *
	 * @param line line to be checked
	 * @return wether to test it or not
	 */
	public boolean isIncluded(AbstractIndexedLine line);

	/**
	 * get the value from a list of lines
	 * @param line to extract the value from
	 * @return requested value
	 */
	public Object getValue(List<? extends AbstractIndexedLine> lines);

}
