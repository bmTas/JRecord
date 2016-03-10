package net.sf.JRecord.def.IO.builders;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;

/**
 * Class to create an empty Line
 * @author Bruce Martin
 *
 */
public interface INewLineCreator {

	public abstract AbstractLine newLine() throws IOException;

}
