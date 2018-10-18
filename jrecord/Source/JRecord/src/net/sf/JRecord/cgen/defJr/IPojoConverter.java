package net.sf.JRecord.cgen.defJr;

import net.sf.JRecord.cgen.def.IDeserializer;
import net.sf.JRecord.cgen.def.ISerializer;


/**
 * This will<ul>
 * <li>Convert a Pojo class to either a JRecord-Line or an Array of Bytes
 * <li>Convert an array of bytes to a Pojo
 * <li>Update a Line with a Pojo's values.
 * </ul>
 * 
 * 
 * @author Bruce Martin
 *
 * @param <Pojo>
 */
public interface IPojoConverter<Pojo> extends ISerializer<Pojo>, IDeserializer<Pojo>, IToPojo<Pojo>, IUpdateLine<Pojo> {

}
