package net.sf.JRecord.schema;

import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;

public interface IItemUpdateDetails {

	public IArrayItemCheck getArrayCheck();
	public IWriteCheck getWriteCheck();
	public IFormatField getFormatField(); 
	public IRedefineSelection getRedefineSelection();

}