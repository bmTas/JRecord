package net.sf.JRecord.External.Def;

import java.util.List;

import net.sf.JRecord.Common.AbstractRecordX;
import net.sf.JRecord.Common.IFieldDetail;

public interface IDependingOnIndexDtls {

	DependingOn getDependingOn();

	int getIndex();

	void updateFieldInChildren(AbstractRecordX<? extends IFieldDetail> rec);

	List<DependingOn> getChildren();

}