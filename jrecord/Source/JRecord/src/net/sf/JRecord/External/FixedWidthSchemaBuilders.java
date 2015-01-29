package net.sf.JRecord.External;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;

public final class FixedWidthSchemaBuilders {

	
	public interface IByPositionBuilder extends IBasicSchemaBuilder {
		
		/**
		 * Add a field specifying the starting position of the field
		 * 
		 * @param name field name
		 * @param type Field Type
		 * @param pos starting position of the field
		 * @param decimal number of decimal places (fixed length numeric fields)
		 * 
		 * @return This schema builder so more fields can be added.
		 */
		public IByPositionBuilder addFieldByPosition(String name, int type, int pos, int decimal);

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
		public ExternalRecord addFieldByPosition(String name, int type, int pos, int length, int decimal);
	}

	public interface IByLengthBuilder extends IBasicSchemaBuilder {
		public IByLengthBuilder addFieldByLength(String name, int type, int length, int decimal);
	}

	private interface IBasicSchemaBuilder {

		/**
		 * Convert to schema (LayoutDetail)
		 * @return schema in the form of LayoutDetail
		 * @throws RecordException
		 */
		public LayoutDetail asLayoutDetail() throws RecordException;

		/**
		 * Used in interface to convert back to ExternalRecord
		 * @return this ExternalRecord
		 */
		public ExternalRecord asExternalRecord();
	}
}
