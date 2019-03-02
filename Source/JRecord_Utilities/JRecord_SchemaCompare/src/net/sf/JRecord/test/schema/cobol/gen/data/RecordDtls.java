package net.sf.JRecord.test.schema.cobol.gen.data;

import java.util.ArrayList;
import java.util.List;

/**
 * This class holds a 'Record' that has been read from
 * the layout file by ReadJrSchema (Cobol Copybook: JR_Schema_Test.cbl)
 * 
 * @author Bruce Martin
 *
 */
public final class RecordDtls {
	public final LineSchemaRecordRecordJR record;
	public final List<LineFieldRecordJR> fields = new ArrayList<LineFieldRecordJR>();
	
	public RecordDtls(LineSchemaRecordRecordJR record) {
		super();
		this.record = record;
	}
}
