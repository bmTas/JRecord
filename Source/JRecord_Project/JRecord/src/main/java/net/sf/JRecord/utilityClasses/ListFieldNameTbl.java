/**
 * 
 */
package net.sf.JRecord.utilityClasses;

import java.util.List;

import net.sf.JRecord.util.copy.IFieldNameTbl;

/**
 * @author brucemartin
 *
 */
public class ListFieldNameTbl implements IFieldNameTbl {

	private final List<String> fieldNames;
	
	
	public ListFieldNameTbl(List<String> fieldNames) {
		super();
		this.fieldNames = fieldNames;
	}

	@Override
	public int getRowCount() {
		return fieldNames.size();
	}

	@Override
	public int getColumnCount(int row) {
		return 1;
	}

	@Override
	public String getName(int row, int column) {
		return fieldNames.get(row);
	}

}
