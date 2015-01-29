package net.sf.JRecord.External;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;

public interface ICsvSchemaBuilder {

	/**
	 * Add a Csv field to the schema
	 * @param name field name
	 * @param type Field type
	 * @param decimal how many decimals (for fixed length numeric types)
	 * @return this Schema-builder so other fields can be added
	 */
	public ICsvSchemaBuilder addCsvField(String name, int type, int decimal);
	
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
