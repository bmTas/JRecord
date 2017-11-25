package net.sf.JRecord.cgen.def;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;

/**
 * Extended Array Definition
 * @author bruce
 *
 */
public interface IArrayExtended extends IArrayAnyDimension {
	
	/**
	 * 
	 * @param index
	 * @return
	 */
	public abstract IFieldDetail getField(IIndex index);

	/**
	 * define the field for a index;
	 * @param index 
	 * @param fieldDefinition
	 */
	public void setField(IIndex index, FieldDetail fieldDefinition);
}
