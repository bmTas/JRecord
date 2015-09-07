package net.sf.JRecord.cg.schema;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalGroupSelection;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.cg.common.CCode;


/**
 * Class to describe one record type in a file for use in code generation
 * @author Bruce Martin
 *
 */
public class RecordDef extends JavaDetails {
	private final RecordDetail record;
	
	private final ArrayList<FieldDef> fields = new ArrayList<FieldDef>();
	
	private final String RecordSelectionStr;

	/**
	 * Class to describe one record type in a file for use in code generation
	 * 
	 * @param record standard Record Description
	 */
	public RecordDef(RecordDetail record) {
		super(record.getRecordName()); 
		this.record = record;
		
		int fieldCount = record.getFieldCount();
		HashMap<String, Integer> fieldsUsed = new HashMap<String, Integer>(fieldCount * 3);
		FieldDetail field;
		String fldName, lcFldName;
		
		fields.ensureCapacity(fieldCount);
		
		for (int i = 0; i < record.getFieldCount(); i++) {
			field = record.getField(i);
			fldName = field.getName();
			lcFldName = field.getName().toLowerCase();
			Integer num = fieldsUsed.get(lcFldName);
			if (num == null) {
				num = Integer.valueOf(1);
			} else {
				fldName = fldName + num;
				num = num + 1;
			}
			fieldsUsed.put(lcFldName, num);
			
			fields.add(new FieldDef(fldName, field));
		}
		
		StringBuilder b = new StringBuilder(30);
		expandSel(b, record.getRecordSelection().getRecSel(), 0);
		RecordSelectionStr = b.toString();
	}

	public void expandSel(StringBuilder b, ExternalSelection sel, int level) {
		char[] indent = new char[level * 3 + 5];
		Arrays.fill(indent, ' ');
		
		b.append(indent);
		if (sel == null) {
			
		} else if (sel instanceof ExternalGroupSelection) {
			@SuppressWarnings("rawtypes")
			ExternalGroupSelection eg = (ExternalGroupSelection) sel;
			char sep = '(';
			
			if (eg.getType() == ExternalSelection.TYPE_AND) {
				b.append("ExternalGroupSelection.newAnd\n");
			} else {
				b.append("ExternalGroupSelection.newOr\n");
			}
			for (int i = 0; i < eg.getElementCount(); i++) {
				expandSel(b.append(sep), eg.get(i), level+1);
				sep = ',';
			}
			b.append(indent).append("   )\n");
		} else if (sel instanceof ExternalFieldSelection) {
			ExternalFieldSelection ef = (ExternalFieldSelection) sel;
			b.append("new ExternalFieldSelection(\"" 
						+ ef.getFieldName() + "\", \"" + ef.getFieldValue()+ "\", \"" + ef.getOperator() + "\")\n");
		} else {
			throw new RuntimeException("Invalid Record Selection class: " + sel.getClass().getName());
		}
	}
	
	
	/**
	 * @return the record
	 */
	public final RecordDetail getRecord() {
		return record;
	}

	/**
	 * @return the fields
	 */
	public final List<FieldDef> getFields() {
		return fields;
	}
	
	/**
	 * @return the fields
	 */
	public final List<FieldDef> getFields(int limit) {
		if (fields.size() <= limit) {
			return fields;
		}
		ArrayList<FieldDef> r = new ArrayList<>(limit);
		for (int i = 0; i < limit; i++) {
			r.add(fields.get(i));
		}
		return r;
	}

	
	public String getJRecordRecordType() {
		return CCode.getRecordTypeName(record.getRecordType());
	}

	/**
	 * @return the recordSelectionStr
	 */
	public final String getRecordSelectionStr() {
		return RecordSelectionStr;
	}
}
