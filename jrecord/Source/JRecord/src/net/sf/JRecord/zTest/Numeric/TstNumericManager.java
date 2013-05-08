package net.sf.JRecord.zTest.Numeric;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import junit.framework.TestCase;

public class TstNumericManager extends TestCase {

	public void testManagerVbFileStructure() {
		int[] openCob = {
				Convert.FMT_OPEN_COBOL,     Convert.FMT_OPEN_COBOL_BE,
				Convert.FMT_OPEN_COBOL_MVS, Convert.FMT_OPEN_COBOL_MVS_BE,
				Convert.FMT_OC_MICRO_FOCUS, Convert.FMT_OC_MICRO_FOCUS_BE,
				Convert.FMT_FS2000,         Convert.FMT_FS2000_BE,
		};
		ConversionManager m = ConversionManager.getInstance();
		Convert c;

		for (int i = 0; i < openCob.length; i++) {
			c = m.getConverter4code(openCob[i]);
			assertEquals("Open Cobol " + c.getName() + " " + i,
					Constants.IO_VB_OPEN_COBOL, c.getFileStructure(true, true));
		}
		assertEquals("Mainframe", Constants.IO_VB, m.getConverter4code(Convert.FMT_MAINFRAME).getFileStructure(true, true));
		assertEquals("Fujitsu", Constants.IO_VB_FUJITSU, m.getConverter4code(Convert.FMT_FUJITSU).getFileStructure(true, true));
	}

}
