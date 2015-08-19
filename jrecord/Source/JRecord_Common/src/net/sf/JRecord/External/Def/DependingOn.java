package net.sf.JRecord.External.Def;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import net.sf.JRecord.Common.AbstractRecordX;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;

public class DependingOn {
	private static final Pattern OF_SPLIT = Pattern.compile("\\sOF\\s", Pattern.CASE_INSENSITIVE);
	private final String variableName;
	private final int position, occursLength, occursMax, occursMaxLength;
	private List<DependingOn> children = null;
	private IFieldDetail field;
//	private DependingOn parent = null;
	
	 
	public DependingOn(String variableName, int position, int occursLength, int occursMax) {
		super();
		this.variableName = variableName;
		this.position = position;
		this.occursLength = occursLength;
		this.occursMax = occursMax;
		this.occursMaxLength = occursLength * occursMax;
	}

	/**
	 * @return the children
	 */
	public final List<DependingOn> getChildren() {
		return children;
	}

	/**
	 * @return the variableName
	 */
	public final String getVariableName() {
		return variableName;
	}

	/**
	 * @return the position
	 */
	public final int getPosition() {
		return position;
	}
	/**
	 * @return the position
	 */
	public final int getEnd() {
		return position + occursMaxLength - 1;
	}

	/**
	 * @return the occursLength
	 */
	public final int getOccursLength() {
		return occursLength;
	}

	/**
	 * @return the occursMax
	 */
	public final int getOccursMax() {
		return occursMax;
	}


	/**
	 * @return the field
	 */
	public final IFieldDetail getField() {
		return field;
	}

	/**
	 * @return the occursMaxLength
	 */
	public final int getOccursMaxLength() {
		return occursMaxLength;
	}
	
	public final void addChild(DependingOn child) {
		children = addChild(children, child);
	}
	
	
	public static List<DependingOn> addChild(List<DependingOn> childList, DependingOn child) {
		if (childList == null) {
			childList = new ArrayList<DependingOn>(3);
		} else { 
			for (DependingOn c : childList) {
				if (c.position <= child.position
				&& c.position + c.occursMaxLength > child.position) {
//					child.parent = c;
					c.addChild(child);
					return childList;
				}
			}
		}
		
		childList.add(child);
		return childList;
	}

	
	public void updateField(AbstractRecordX<? extends IFieldDetail> rec) {
		try {
			String[] groups = OF_SPLIT.split(variableName);
			if (groups == null || groups.length < 2) {
				field = rec.getField(variableName);
			} else {
				String[] groupsNS = new String[groups.length];
				for (int i = 0; i < groups.length; i++) {
					groupsNS[groups.length - i] = groups[i];
				}
				field = rec.getGroupField(groupsNS);
			}
		} catch (Exception e) {
			throw new RuntimeException("Error With Occurs Depending On Field: " + variableName, e);
		}
		
		if (field == null) {
			throw new RuntimeException("Error With Occurs Depending On Field: " + variableName);
		}
		
		if (field instanceof FieldDetail) {
			((FieldDetail) field).setOccursDependingOnValue(true);
		}
		
		if (children != null) {
			for (DependingOn c : children) {
				c.updateField(rec);
			}
		}
	}
}
