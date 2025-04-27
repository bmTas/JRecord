package net.sf.JRecord.zTest.Common;

public class CommonCodeTestCopybooksAndData {
	public static final String DTAR020_COPYBOOK
					= "              03  DTAR020-KCODE-STORE-KEY.                        \n"
					+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).           \n"
					+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3. \n"
					+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3. \n"
					+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3. \n"
					+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3. \n"
					+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3. \n";
	
	public static final String DTAR020_TST1_DATA_FILE = TstConstants.SAMPLE_DIRECTORY + "DTAR020_tst1.bin";

}
