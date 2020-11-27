package net.sf.JRecord.External.Def;


/**
 * used to notify parent record of an update to a field
 * @author Bruce Martin
 *
 */
public interface IFieldUpdatedListner {
	/**
	 * Notify of field update
	 * @param field updated field
	 */
	public void fieldUpdated(ExternalField field);
}
