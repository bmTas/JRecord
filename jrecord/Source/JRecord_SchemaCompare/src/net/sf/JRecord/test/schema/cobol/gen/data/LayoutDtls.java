package net.sf.JRecord.test.schema.cobol.gen.data;

import java.util.ArrayList;
import java.util.List;

/**
 * This class Holds one 'Layout' that has been read from the Layout file
 * (Cobol Copybook=JR_Schema_Test.cbl) by {@link net.sf.JRecord.test.schema.cobol.io.ReadJrSchema}
 * 
 * @author Bruce Martin
 *
 */
public final class LayoutDtls {
	public final LineCopybookRecordJR copyBookDtls;
	public final LineDialectRecordJR dialectDtls;
	
	public final List<RecordDtls> records = new ArrayList<RecordDtls>();

	public LayoutDtls(LineCopybookRecordJR copyBookDtls,
			LineDialectRecordJR dialectDtls) {
		super();
		this.copyBookDtls = copyBookDtls;
		this.dialectDtls = dialectDtls;
	}
}
