package net.sf.JRecord.ExternalRecordSelection;

import java.util.ArrayList;

public class ExternalGroupSelection<fs extends ExternalSelection> implements ExternalSelection {

	private ArrayList<fs> items ;
	private int type = ExternalSelection.TYPE_AND;



	public ExternalGroupSelection() {
		super();
		items = new ArrayList<fs>();
	}


	public ExternalGroupSelection(int size) {
		super();
		items = new ArrayList<fs>(size);
	}

	/**
	 * @param e
	 * @return
	 * @see java.util.ArrayList#add(java.lang.Object)
	 */
	public boolean add(fs e) {
		return items.add(e);
	}

	/**
	 * @param index
	 * @return
	 * @see java.util.ArrayList#get(int)
	 */
	public fs get(int index) {
		return items.get(index);
	}

	/**
	 * @return
	 * @see java.util.ArrayList#size()
	 */
	public int size() {
		return items.size();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.RecordSelection.ExternalSelection#getType()
	 */
	@Override
	public int getType() {
		return type;
	}


	/**
	 * @param type the type to set
	 */
	public void setType(int type) {
		this.type = type;
	}
	
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
	 * @param idx index
	 * @param selectionj
	 * @return
	 * @see java.util.ArrayList#set(int, java.lang.Object)
	 */
	public fs set(int idx, fs selection) {
		return items.set(idx, selection);
	}

}
