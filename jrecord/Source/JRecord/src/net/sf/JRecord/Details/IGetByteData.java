package net.sf.JRecord.Details;

/**
 * Class can return a Record (line) as an array of bytes
 * @author Bruce Martin
 *
 */
public interface IGetByteData {

	/**
	 * @return Returns the record (line) as an array of bytes.
	 */
	public abstract byte[] getData();

}