package net.sf.JRecord.util.copy;

import java.util.List;

import net.sf.JRecord.Details.AbstractLine;

/**
 * 
 * @author brucemartin
 *
 */
public interface IUpdateLine {
	List<AbstractLine> updateLine(AbstractLine inputLine);
}
