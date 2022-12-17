package net.sf.JRecord.util.copy;

public interface IFieldNameTbl {

	int getRowCount();
	int getColumnCount(int row);
	String getName(int row, int column);
}
