package net.sf.JRecord.cg.schema;

import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.cg.common.CCode;

/**
 * Class to represent a schema for use in Code Generation
 * @author Bruce Martin
 *
 */
public class LayoutDef extends JavaDetails {
	private final LayoutDetail schema;
	private final ArrayList<RecordDef> records = new ArrayList<RecordDef>();
	
	public LayoutDef(LayoutDetail schema) {
		super(schema.getLayoutName());
		this.schema = schema;
		
		records.ensureCapacity(schema.getRecordCount());
		for (int i = 0; i < schema.getRecordCount(); i++) {
			records.add(new RecordDef( schema.getRecord(i) ));
		}
	}

	/**
	 * get the actual schema
	 * @return the schema
	 */
	public final LayoutDetail getSchema() {
		return schema;
	}

	/**
	 * get the Records in the schema
	 * @return the records
	 */
	public final List<RecordDef> getRecords() {
		return records;
	}
	
	
	public String getJRecordLayoutType() {
		return CCode.getRecordTypeName(schema.getLayoutType());
	}
	
	public String getJRecordIoType() {
		return CCode.getJRecordIoTypeName(schema.getFileStructure());
	}

}
