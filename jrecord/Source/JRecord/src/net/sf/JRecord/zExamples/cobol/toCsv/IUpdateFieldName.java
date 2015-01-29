package net.sf.JRecord.zExamples.cobol.toCsv;

public interface IUpdateFieldName {

	/**
	 * Convert Cobol/RecordEditor Name to standard name
	 *
	 * @param name current field name
	 *
	 * @return new name
	 */
	public abstract String updateName(String name);

}