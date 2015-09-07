package net.sf.JRecord.External;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;

public interface IBasicSchemaBuilder {
	

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

