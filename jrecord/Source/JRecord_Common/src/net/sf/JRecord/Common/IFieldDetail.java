package net.sf.JRecord.Common;

import net.sf.JRecord.Option.IOptionResult;
import net.sf.JRecord.Option.IOptionType;

/**
 * Description of one field in a Record (Line)
 *
 * @author Bruce Martin
 *
 */
public interface IFieldDetail {

	public abstract IFieldDetail setPosLen(final int pPosition, final int pLength);

	public abstract IFieldDetail setPosOnly(final int pPosition);

	public abstract int getDecimal();

	public abstract int getLen();

	public abstract String getName();
	
	public abstract String getLookupName();
	
	public abstract void setLookupName(String lookupName);

	public abstract int getPos();

	public abstract int getType();

	public abstract String getDescription();

	@Deprecated
	public abstract int getEnd();

	public abstract boolean isFixedFormat();

	public abstract String getFontName();

	public abstract int getFormat();

	public abstract String getParamater();

	public abstract String getQuote();

	public abstract AbstractRecord getRecord();

	public abstract void setRecord(AbstractRecord record);

	public abstract void setNameType(String newName, int newType);

	public abstract Object getDefaultValue();

	public abstract void setDefaultValue(Object defaultValue);
	
	public abstract IOptionResult getOption(IOptionType type);

	public abstract int calculateActualPosition(AbstractIndexedLine line);

//	public abstract int calculateActualLength(AbstractIndexedLine line);
	
	public abstract int calculateActualEnd(AbstractIndexedLine line);

}