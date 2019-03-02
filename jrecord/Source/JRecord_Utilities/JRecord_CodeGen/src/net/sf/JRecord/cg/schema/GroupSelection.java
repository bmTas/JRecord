/**
 * 
 */
package net.sf.JRecord.cg.schema;

import java.util.ArrayList;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

/**
 * @author Bruce01
 *
 */
public class GroupSelection implements ExternalSelection {

	private final int type; //= ExternalSelection.TYPE_AND;
	private final String booleanOperator;
	private final ArrayList<ExternalSelection> items; // = new ArrayList<ExternalSelection>();
	
	
	protected GroupSelection(int type, String booleanOperator,
			ArrayList<ExternalSelection> items) {
		super();
		this.type = type;
		this.booleanOperator = booleanOperator;
		this.items = items;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.ExternalRecordSelection.ExternalSelection#getType()
	 */
	@Override
	public int getType() {
		return type;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.ExternalRecordSelection.ExternalSelection#getSize()
	 */
	@Override
	public int getSize() {
		return items.size();
	}
	
	
	@Override
	public int getElementCount() {
		int count = 0;
		
		for (ExternalSelection s : items) {
			count += s.getElementCount();
		}
		return count;
	}

	/**
	 * @return the booleanOperator 
	 */
	public final String getBooleanOperator() {
		return booleanOperator;
	}

	/**
	 * @return the items
	 */
	public final ArrayList<ExternalSelection> getItems() {
		return items;
	}

}
