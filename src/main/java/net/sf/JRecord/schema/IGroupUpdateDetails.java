package net.sf.JRecord.schema;

import java.util.List;

public interface IGroupUpdateDetails {

	IItemUpdateDetails getUpdateDetails(List<String> names);
	//	
	//	public IArrayItemCheck getArrayCheck(String name) {
	//		UpdateDetails ud = getUpdateDetails(name);
	//		return ud == null ? null : ud.arrayCheck;
	//	}

}