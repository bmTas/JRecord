package net.sf.JRecord.IO;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;

public interface IReadLine {

	public abstract AbstractLine read() throws IOException;
}
