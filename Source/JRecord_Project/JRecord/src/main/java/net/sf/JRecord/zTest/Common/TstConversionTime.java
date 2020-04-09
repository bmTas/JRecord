package net.sf.JRecord.zTest.Common;

import net.sf.JRecord.Common.Conversion;

public class TstConversionTime {

	public TstConversionTime() {
		// TODO Auto-generated constructor stub
	}
	
	private void run1(int num) {
		byte[] rec = new byte[8];
		for (int i = 0; i < num; i++) {
			Conversion.setLong(rec, 0, 4, i * 3, false);
		}
	}
	
	private void run2(int num) {
		byte[] rec = new byte[8];
		for (int i = 0; i < num; i++) {
			Conversion.setLongLow2High(rec, 0, 4, i * 3, false);
		}
	}

	public static void main(String[] args) {
		TstConversionTime tst = new TstConversionTime(); 
		
		tst.run1(800000);
		
		long t = System.currentTimeMillis();
		tst.run1(900000000);
		System.out.println("** Run1: " + (System.currentTimeMillis() - t));
		
		tst.run2(800000);
		
		t = System.currentTimeMillis();
		tst.run2(900000000);
		System.out.println("** Run2: " + (System.currentTimeMillis() - t));
	}

}
