package net.sf.JRecord.IO.builders;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;

/**
 * IO Builder based on an existing File-Schema (LayoutDetail);
 * 
 * @author Bruce Martin
 *
 */
public class SchemaIOBuilder extends CblIOBuilderBase {

	public static ISchemaIOBuilder newSchemaIOBuilder(LayoutDetail schema) {
		return new SchemaIOBuilder(schema);
	}
	
	private SchemaIOBuilder(LayoutDetail schema) {
		super(schema);
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#getExternalRecordImpl()
	 */
	@Override
	protected ExternalRecord getExternalRecordImpl()  {
		throw new RuntimeException("Error: this method should not get called in SchemaIOBuilder");
	}

}
