package net.sf.JRecord.External;

import net.sf.JRecord.External.base.IExernalRecordBuilder;

public class ExternalRecordBuilder implements IExernalRecordBuilder<ExternalRecord> {

	@Override
	public ExternalRecord getNullRecord(String pRecordName, int recordType, String fontName) {
		return ExternalRecord.getNullRecord(pRecordName, recordType, fontName);
	}

}
