package net.sf.JRecord.Common;

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

	public abstract int getPos();

	public abstract int getType();

	public abstract String getDescription();

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

}