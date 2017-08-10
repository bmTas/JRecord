package net.sf.cobolToJson.impl;


/**
 * Store user supplied Record Selection option
 * @author Bruce Martin
 *
 */
public class RecordSelect {

	public final String recordName, fieldName, value;
//	private final String inStr; //used in testing
	
	public RecordSelect(String arg) {
//		inStr = arg; //used in testing
		arg = arg.trim();
		
		String rn = arg, fn = null, v = null;
		int pos = arg.indexOf(' ');
		if (pos >= 0) {
			rn = arg.substring(0, pos).trim();
			arg = arg.substring(pos).trim();
			
			pos = arg.indexOf('=');
			if (pos < 0) {
				pos = arg.indexOf(' ');
			}
			if (pos >= 0) {
				fn = arg.substring(0, pos).trim();
				v = arg.substring(pos+1).trim();
			}
		}
		recordName = rn;
		fieldName = fn;
		value = v;
	}
	
	public final boolean ok() {
		return  fieldName != null && fieldName.length() > 0 && fieldName.indexOf(' ') < 0
			&&	value != null && value.length() > 0; 
	}
	
	
//  Test Code:
	
//	private void print() {
//		System.out.println("==>" + inStr + "<-\t->" + recordName + "<- ->" + fieldName + "< == >" + value + "<");
//	}
//	
//	public static void main(String[] args) {
//		(new RecordSelect(" aa ")).print();
//		(new RecordSelect("aa bb")).print();
//		(new RecordSelect("aa bb cc")).print();
//		(new RecordSelect(" aa bb cc ")).print();
//		(new RecordSelect(" aa  bb  cc ")).print();
//		(new RecordSelect("   aa    bb    cc  ")).print();
//		
//		(new RecordSelect("aa bb=cc")).print();
//		(new RecordSelect(" aa bb=cc ")).print();
//		(new RecordSelect("  aa bb =cc  ")).print();
//		(new RecordSelect("  aa bb= cc  ")).print();
//		(new RecordSelect("  aa bb = cc  ")).print();
//		(new RecordSelect("  aa bb  = cc  ")).print();
//		(new RecordSelect("  aa bb=  cc  ")).print();
//		(new RecordSelect("  aa bb  =  cc  ")).print();
//	}
}
