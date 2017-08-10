package net.sf.JRecord.cgen.def;

import net.sf.JRecord.Details.AbstractLine;


/**
 * This will<ul>
 * <li>Convert a Pojo class to either a JRecord-Line or an Array of Bytes
 * <li>Convert an array of bytes to a Pojo
 * <li>Update a Line with a Pojo's values.
 * </ul>
 * 
 * 
 * @author bruce
 *
 * @param <Pojo>
 */
public interface IPojoConverter<Pojo> extends ISerializer<Pojo>, IDeserializer<Pojo> {

	
	/**
	 * Convert a JRecord line to a Pojo.
	 * @param line JRecord line
	 * @return Pojo
	 */
	public Pojo toPojo(AbstractLine line);
	
	/**
	 * This method copies the Pojo values to a JRecord line
	 * @param line JRecord line to be updated
	 * @param pojo 
	 */
	public void updateLine(AbstractLine line, Pojo pojo);
}
