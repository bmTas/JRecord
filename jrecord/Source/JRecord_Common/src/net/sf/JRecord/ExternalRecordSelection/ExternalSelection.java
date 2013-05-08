package net.sf.JRecord.ExternalRecordSelection;

public interface ExternalSelection {

	public static final int TYPE_ATOM = 1;
	public static final int TYPE_AND = 2;
	public static final int TYPE_OR = 3;
	public static final int TYPE_AND_OR = 4;
	public static final int TYPE_OR_AND = 5;
	
	public int getType();
	
	public int getSize();

	int getElementCount();
}
