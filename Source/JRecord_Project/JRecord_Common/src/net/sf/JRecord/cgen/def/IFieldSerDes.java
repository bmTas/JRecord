package net.sf.JRecord.cgen.def;

import net.sf.JRecord.Common.AbstractFieldValue;

/**
 * Define a class to convert a field to/from a java class
 * i.e. serializer/deserialise
 * 
 * @author Bruce Martin
 *
 * @param <FieldType>
 */
public interface IFieldSerDes<FieldType> {

	public FieldType getFromField(AbstractFieldValue dateFld) ;
	public void setField(AbstractFieldValue dateFld, FieldType date);

}
