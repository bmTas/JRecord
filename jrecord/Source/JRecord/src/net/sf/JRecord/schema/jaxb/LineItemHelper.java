package net.sf.JRecord.schema.jaxb;

import java.util.Arrays;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.IFieldValue;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.Def.DependingOnDefinition;


/**
 * This Helper class to access Line using the item tree;
 * It consolidates access and logic for accessing a line from the item tree in 
 * a single class. It also keeps this logic inside JRecord.
 * 
 * @author Bruce Martin
 *
 */
public class LineItemHelper {
	private AbstractLine line;
	private final int[] sizes;
	
	public LineItemHelper(LayoutDetail schema) {
		
		int numArraySizeFields = 0;
		for (int i = 0; i < schema.getRecordCount(); i++) {
			DependingOnDefinition dependingOn = schema.getRecord(i).getDependingOn();
			if (dependingOn != null) {
				numArraySizeFields = Math.max(numArraySizeFields, dependingOn.getSizeFieldCount());
			}
		}
		
		int[] sizes = null;
		if (numArraySizeFields > 0) {
			sizes = new int[numArraySizeFields];
		}
		this.sizes = sizes;
	}
	
	/**
	 * Get the field-value of an item
	 * 
	 * @param item item to retrieve
	 * @param indexs index's if present
	 * 
	 * @return requested field value
	 */
	public IFieldValue getFieldValue(IItem item, int[] indexs) {
		IFieldValue ret;
		if (indexs == null || indexs.length == 0) {
			ret = line.getFieldValue(item.getFieldDefinition());
		} else {
			ret = line.getFieldValue(item.getArrayDefinition().getField(indexs));
		}
		@SuppressWarnings("deprecation")
		int idx = item.getSaveIndex();
		if (ret.isNumeric() && idx >= 0) {
			try {
				sizes[idx] = ret.asInt();
			} catch (Exception e) {
				System.err.println();
				System.err.print(">> " + item.getName() + " ~ " + ret.asString() + " - " + idx + " : ");
				if (indexs != null) {
					for (int i = 0; i < indexs.length; i++) {
						System.err.print("\t" + indexs[i]);
					}
				}
				System.err.println();
				System.err.println();
				//e.printStackTrace();
			}
		}
		
		return ret;
	}
	
	public int checkArrayIndex( IItem item, int[] indexs, int index) {
		return item.getArrayValidation().checkItem(line, item, indexs, index);
	}
	
	public int getArrayCount(IItem item, int[] indexs) {
		int num = item.getOccurs();
		
		@SuppressWarnings("deprecation")
		int idx = item.getODArraySizeIdx();
		if (sizes != null && idx >= 0 && sizes[idx] >= 0) {
			num = sizes[idx];
		} /*else {
			String dependingOn = item.getDependingOn();
			if (dependingOn != null && dependingOn.length() > 0) {
				
			}
		}*/
		if (item.getArrayValidation() != null) {
			num = item.getArrayValidation().getCount(line, item, indexs, num);
		}
		
		return num;
	}
	
	
	/**
	 * @return the line
	 */
	public final AbstractLine getLine() {
		return line;
	}
	/**
	 * @param line the line to set
	 */
	public final LineItemHelper setLine(AbstractLine line) {
		this.line = line;
		if (sizes != null) {
			Arrays.fill(sizes, -1);
		}
		
		return this;
	}
}
