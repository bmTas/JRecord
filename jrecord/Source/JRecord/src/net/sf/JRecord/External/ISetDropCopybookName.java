package net.sf.JRecord.External;

public interface ISetDropCopybookName {

	/**
	 * @param dropCopybookFromFieldNames the dropCopybookFromFieldNames to set
	 */
	public abstract void setDropCopybookFromFieldNames(
			boolean dropCopybookFromFieldNames);

	/**
	 * wether to keep the source cb2xml document
	 * 
	 * @param saveCb2xml wether to keep the source cb2xml document
	 */
	public abstract void setSaveCb2xmlDocument(boolean saveCb2xml);

}