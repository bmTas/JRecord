package net.sf.JRecord.occursDepending;

import java.util.Arrays;

import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.DependingOnDefinition;
import net.sf.JRecord.External.Def.DependingOnDtls;

/**
 * Holds field sizes / lookup details for a line.
 * This is used to speedup occurs-depending calculation
 * 
 * @author Bruce Martin
 *
 */
public class ODLastLookupDetails {
	private static final SavedDependingOnDtls DEFAULT_DEP_DTLS = new SavedDependingOnDtls(null, 0);
	
	final AbstractIndexedLine line;
	final int[] arraySizes;
	//int arraySize = -1;
	SavedDependingOnDtls savedDependOnDtls = DEFAULT_DEP_DTLS;
	
	
	public ODLastLookupDetails(AbstractIndexedLine line) {
		super();
		this.line = line;
		this.arraySizes = null;
	}

	
	public ODLastLookupDetails(AbstractIndexedLine line, DependingOnDefinition dependingOn) {
		super();
		this.line = line;
		this.arraySizes = new int[dependingOn.getSizeFieldCount() + 1];
		
		Arrays.fill(this.arraySizes, Integer.MIN_VALUE);
	}
	
	public int getFieldValue(DependingOn dependOn) {
		int fieldNumber = dependOn.getFieldNumber();
		
		if (fieldNumber < 0 || fieldNumber >= arraySizes.length) {
			String valStr = getFieldVal(dependOn);
			if (valStr.length() == 0) {
				return 0;
			}
			return Integer.parseInt(valStr);
		}
		if (arraySizes[fieldNumber] < 0) {
			String valStr = getFieldVal(dependOn);
			if (valStr.length() == 0) {
				return 0;
				//throw new RuntimeException("Can not retrieve field: " + dependOn.getVariableName());
			}
			arraySizes[fieldNumber] = Integer.parseInt(valStr);
		}
		
		return arraySizes[fieldNumber];
	}
	
	public  String getFieldVal(DependingOn dependOn) {
		Object value = line.getField(dependOn.getField());
		return value==null? "" : value.toString().trim();
	}
	
	
	void setDependingOnDtls(DependingOnDtls dependOnDtls, int adjustment) {
		this.savedDependOnDtls = new SavedDependingOnDtls(dependOnDtls, adjustment);
	}
	
	void clearSizeFieldNumber(int fieldNumber) {
		arraySizes[fieldNumber] = Integer.MIN_VALUE;
		savedDependOnDtls = DEFAULT_DEP_DTLS;
	}
	
//	public void checkForSizeFieldUpdate(DependingOnDefinition.SizeField sizeFld, IFieldDetail fld, Object value) {
//		
//		this.arraySizes[sizeFld.fieldNumber] = Integer.MIN_VALUE;
//		
////		if (TypeManager.isNumeric(fld.getType()) && fld.getDecimal() == 0 && value != null) {
////			if (fld instanceof FieldDetail && ((FieldDetail) fld).getRecord() == parent) {
////				if (value instanceof Number) {
////					this.arraySizes[sizeFld.fieldNumber] = ((Number) value).intValue();
////				} else {
////					try {
////						this.arraySizes[sizeFld.fieldNumber] = Integer.parseInt(value.toString());
////					} catch (Exception e) {
////					}
////				}
////				
////				System.out.print("-----> " + fld.getName() + " " + value);
////			}
////		}
//		
//	}

	
	static class SavedDependingOnDtls {
		final DependingOnDtls dependOnDtls;
		final int adjustment;
		
		public SavedDependingOnDtls(DependingOnDtls dependOnDtls, int adjustment) {
			super();
			this.dependOnDtls = dependOnDtls;
			this.adjustment = adjustment;
		}
		
		
	}
}
