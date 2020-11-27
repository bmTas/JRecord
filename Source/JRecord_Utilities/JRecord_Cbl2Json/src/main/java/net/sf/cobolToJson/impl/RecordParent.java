package net.sf.cobolToJson.impl;

public class RecordParent {

	public final String recordName, parentName;
	//private final String inStr; //used in testing
	
	public RecordParent(String arg) {
		//inStr = arg; //used in testing
		arg = arg.trim();
		
		String rn = arg, pn = null;
		int pos = arg.indexOf(' ');
		if (pos >= 0) {
			rn = arg.substring(0, pos);
			pn = arg.substring(pos).trim();
		}
		recordName = rn;
		parentName = pn;
	}
	
	public final boolean ok() {
		return parentName != null && parentName.length() > 0 && parentName.indexOf(' ') < 0; 
	}
	
	
//  Test Code:
	
//	private void print() {
//		System.out.println("==>" + inStr + "<- ->" + recordName + "<- ->" + parentName + "<");
//	}
//	
//	public static void main(String[] args) {
//		(new RecordParent(" aa ")).print();
//		(new RecordParent("aa bb")).print();
//		(new RecordParent(" aa bb ")).print();
//		(new RecordParent("aa  bb")).print();
//		(new RecordParent(" aa    bb ")).print();
//	}
}
