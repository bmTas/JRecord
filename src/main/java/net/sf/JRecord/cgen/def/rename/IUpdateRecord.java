package net.sf.JRecord.cgen.def.rename;

import java.util.List;

public interface IUpdateRecord {

	public String getRecordName();
	public void fieldUpdated();
	public List<? extends IUpdateField> getUpdateFields();
}
