package net.sf.JRecord.External;


public interface IFixedWidthSchemaBuilder extends IBasicSchemaBuilder {

	/**
	 * Add a field to the Record using the Field position and calculating lengths.
	 * This is normally the last field defined
	 * 
	 * @param name Field name
	 * @param type Field Type
	 * @param pos Fields position in the record
	 * @param length Field length
	 * @param decimal number of decimal places
	 * 
	 * @return This Record.
	 */
	public IFixedWidthSchemaBuilder addFieldByPosition(String name, int type, int pos, int length, int decimal);
	
	/**
	 * Add a field specifying the field length
	 * @param name field name
	 * @param type Field type. Values include:<ul>
	 *  <li>Type.ftChar - character field
	 *  <li>Type.ftNumLeftJustified - left justified namber
	 *  <li>Type.ftNumRightJustified - Right justified number
	 * </ul>
	 * @param length field Length
	 * @param decimal number of decimal places
	 * @return this schema builder (for further updates)
	 */
	public IFixedWidthSchemaBuilder addFieldByLength(String name, int type, int length, int decimal);
	
	/**
	 * Skip a specified number of bytes in the record.
	 * @param numberOfBytes number of bytes to be skipped
	 * @return this schema builder (for further updates)
	 */
	public IFixedWidthSchemaBuilder skipBytes(int numberOfBytes);

	public IFixedWidthSchemaBuilder addField(String name, int type, int pos, int length, int decimal);

	public IFixedWidthSchemaBuilder addFieldByPosition(String name, int type, int pos,
			int decimal);

	public IFixedWidthSchemaBuilder skipFieldPosition(int pos);
	
}
