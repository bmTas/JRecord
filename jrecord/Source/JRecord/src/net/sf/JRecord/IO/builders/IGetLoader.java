package net.sf.JRecord.IO.builders;

import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.Log.AbsSSLogger;

public interface IGetLoader {

	/**
	 * @return the loader
	 */
	public abstract ICopybookLoaderStream getLoader();

	public abstract boolean isDropCopybookNameFromFields();
	
	public abstract String getFont();

	public abstract int getDialect();

	public abstract AbsSSLogger getLog();

	public abstract int getCopybookFileFormat();
}