package net.sf.JRecord.zTest.Cobol;

import java.math.BigDecimal;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.zTest.Common.TstConstants;

/*
 * 
 * 
 */
public class TstCobol extends TestCase {
/*
           03 Num2         pic s9(02)v99 comp.
           03 Num3         pic s9(03)v99 comp.
           03 Num4         pic s9(04)v99 comp.
           03 Num5         pic s9(05)v99 comp.
           03 Num6         pic s9(06)v99 comp.
           03 Num7         pic s9(07)v99 comp.
           03 Num8         pic s9(08)v99 comp.
           03 Num9         pic s9(09)v99 comp.
           03 Num10        pic s9(10)v99 comp.
           03 Num11        pic s9(11)v99 comp.
           03 Num12        pic s9(12)v99 comp.
           03 Num13        pic s9(13)v99 comp.
           03 Num14        pic s9(14)v99 comp.
           03 Num15        pic s9(15)v99 comp.
           03 Num16        pic s9(16)v99 comp.
 */

	private CobolCopybookLoader loader = new CobolCopybookLoader();
	
	private String primaryVar = "NumA";
	private String[] vars = {
			"Num2",  "Num3",  "Num4",  "Num5",  "Num6",  "Num7", 
			"Num8",  "Num9",  "Num10", "Num11", "Num12", "Num13",
			"Num14", "Num15", "Num16",
	};
	private double[] len = {
			02, 03, 04, 05, 06, 07, 8, 9,
			10, 11, 12, 13, 14, 15, 16,
	};
	
	
	
	public void testOpenCobol() throws Exception {
		checkFiles("default/", "cpy", Convert.FMT_OPEN_COBOL);
	}
	
	
	public void testOpenCobolBS2000() throws Exception {
		checkFiles("bs2000/", "bs", Convert.FMT_FS2000);
	}
	
	
	public void testOpenCobolMf() throws Exception {
		checkFiles("mf/", "mf", Convert.FMT_OC_MICRO_FOCUS);
	}

	
	public void testOpenCobolMvs() throws Exception {
		checkFiles("mvs/", "mvs", Convert.FMT_OPEN_COBOL_MVS);
	}

	
	public void testOpenCobolFj() throws Exception {
		checkFiles("Fj/", "fj", Convert.FMT_FUJITSU);
	}
	
	
	
	private void checkFiles(String dir, String pref, int binFormat) throws Exception {
		checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "Comp.cbl",   pref + "Comp.bin", binFormat, false);
		//checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "Comp3.cbl",  pref + "Comp3.bin", binFormat, false);
		//checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "Comp4.cbl",  pref + "Comp4.bin", binFormat, false);
		//checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "Comp5.cbl",  pref + "Comp5.bin", binFormat, false);
		checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "CompPositive.cbl", pref +"CompPositive.bin", binFormat, true);
		checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "CompSync.cbl",     pref + "CompSync.bin", binFormat, false);
		//checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "CompSync1.cbl",    pref + "CompSync1.bin", binFormat, false, len.length - 3);
		checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "Comp5positive.cbl", pref + "Comp5positive.bin", binFormat, true);
		checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "Comp5Sync.cbl",    pref + "Comp5Sync.bin", binFormat, false);
		//checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "TextFields.cbl",   pref + "TextNums.txt", binFormat, false);
		//checkFile(Constants.IO_FIXED_LENGTH, dir, pref + "Comp3.cbl", "TextNums.txt", binFormat, false);
	}
	
	private void checkFile(int structure, String dir, String copybook, String fileName, int binFormat, boolean positive) 
	throws Exception {
		checkFile(structure, dir, copybook, fileName, binFormat, positive, 0);
	}
	
	private void checkFile(int structure, String dir, String copybook, String fileName, int binFormat, boolean positive, int st) throws Exception {
//		String fname = TstConstants.COBOL_TEST_DIR + dir + fileName;
//		String lname = TstConstants.COBOL_COPBOOK_DIR + dir + copybook;
		String fname = TstConstants.COBOL_TEST_DIR + fileName;
		String lname = TstConstants.COBOL_DIRECTORY2 + copybook;
		

		BigDecimal refVal, cmp;
		double log;
		int i, j;
		
		AbstractLineReader reader = LineIOProvider.getInstance().getLineReader(structure);
		AbstractLine line;
		LayoutDetail layout = loader.loadCopyBook(lname, CopybookLoader.SPLIT_NONE, 0, "", binFormat, 0, new TextLog())
											.asLayoutDetail();
			
		System.out.println(fname);
		reader.open(fname, layout);
		
		j = 0;
		while ((line = reader.read()) != null) {
			j += 1;
			refVal = getStandardVal(line);
			if (positive && refVal.doubleValue() < 0) {
				refVal = refVal.multiply(BigDecimal.valueOf(-1));
			}
			log = Math.log10(Math.abs(refVal.doubleValue()));
			
			for (i = st; i < len.length; i++) {
				if (len[i] > log) {
					cmp = line.getFieldValue(vars[i]).asBigDecimal();
					//System.out.println( "--> " + i + " \t" + l + " " + cmp);
					if (! refVal.equals(cmp)) {
						System.out.println( "--> " + i + " \t" + vars[i] + " \t" + log + "\t" + refVal + " " + cmp);
					}
					assertEquals("Error Line: " + j + " " + vars[i], refVal, cmp);
				}
			}
		}
		
		reader.close();
	}
	
	private BigDecimal getStandardVal(AbstractLine line) {
		String s = line.getFieldValue(primaryVar).asString();
		String t;
		StringBuffer b = new StringBuffer("");
		int i;
		
		for (i = 0; i < s.length(); i++) {
			t = s.substring(i, i+1);
			if (! ",".equals(t)) {
				b.append(t);
			}
		}
		
		//System.out.println(" --> " + s + " ==> " + b.toString());
		
		return new BigDecimal(b.toString());
	}
}
