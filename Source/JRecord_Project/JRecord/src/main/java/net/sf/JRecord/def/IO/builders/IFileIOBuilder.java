package net.sf.JRecord.def.IO.builders;


/**
 * Generic Interface for File (e.g. Xml) File Schema's
 * 
 * @author Bruce Martin
 *
 */
public interface IFileIOBuilder extends IIOBuilder {
	
	public abstract IFileIOBuilder setFileOrganization(int fileOrganization);

	public abstract IFileIOBuilder setFont(String font);

	/**
	 * Set the default Font. It may be overridden by the font 
	 * in the File-Schema.
	 * 
	 * @param font default font name
	 * 
	 * @return this
	 */
	public IFileIOBuilder setDefaultFont(String font);
	
//	/**
//	 * Set the default File-Organization (file-Structure) . It may be overridden by the file-organisation 
//	 * in the File-Schema.
//	 * 
//	 * @param fileOrganization default file-Organization 
//	 * 
//	 * @return this
//	 */
//	public IFileIOBuilder setDefaultFileOrganization(int fileOrganization);
}
