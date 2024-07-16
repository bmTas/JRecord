package net.sf.JRecord.Common.layoutSupport;

import java.util.List;

import net.sf.JRecord.Common.IFieldDetail;

public interface ISchemaRecord {

	/**
	 * @return the recordLength
	 */
	int getRecordLength();

	/**
	 * @return the fields
	 */
	List<IFieldDetail> getFields();
	
	/**
	 * 
	 * @return Record-Name
	 */
	String getRecordName();

}