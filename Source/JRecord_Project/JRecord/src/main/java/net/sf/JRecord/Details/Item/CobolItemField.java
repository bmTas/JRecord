package net.sf.JRecord.Details.Item;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.LineFieldCreator;
import net.sf.JRecord.detailsBasic.IItemDetails;


/**
 * Field access via
 *  
 * @author Bruce Martin
 *
 */
public class CobolItemField {

	public final IItemDetails cobolItem;
	private net.sf.JRecord.Details.fieldValue.IFieldValue  fieldValue;
	private final AbstractLine line;
	
	



	public CobolItemField(AbstractLine line, IItemDetails cobolItem) {
		super();
		this.line = line;
		this.cobolItem = cobolItem;
	}



	public net.sf.JRecord.Details.fieldValue.IFieldValue  getFieldValue() {
		IFieldDetail field = cobolItem.getFieldDefinition();
		if (fieldValue == null && field != null) {
			fieldValue = LineFieldCreator.getInstance().newFieldValue(line, field);
		}
		return fieldValue;
	}



	/**
	 * @return the cobolItem
	 */
	public IItemDetails getCobolItem() {
		return cobolItem;
	}


}
