/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.zTest.Numeric;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Numeric.ICopybookDialects;
import junit.framework.TestCase;

public class TstNumericManager extends TestCase {

	public void testManagerVbFileStructure() {
		int[] openCob = {
				ICopybookDialects.FMT_GNU_COBOL,     ICopybookDialects.FMT_GNU_COBOL_BE,
				ICopybookDialects.FMT_GNU_COBOL_MVS, ICopybookDialects.FMT_GNU_COBOL_MVS_BE,
				ICopybookDialects.FMT_OC_MICRO_FOCUS, ICopybookDialects.FMT_OC_MICRO_FOCUS_BE,
				ICopybookDialects.FMT_FS2000,         ICopybookDialects.FMT_FS2000_BE,
		};
		ConversionManager m = ConversionManager.getInstance();
		Convert c;

		for (int i = 0; i < openCob.length; i++) {
			c = m.getConverter4code(openCob[i]);
			assertEquals("GNU Cobol " + c.getName() + " " + i,
					Constants.IO_VB_GNU_COBOL, c.getFileStructure(true, true));
		}
		assertEquals("Mainframe", Constants.IO_VB, m.getConverter4code(ICopybookDialects.FMT_MAINFRAME).getFileStructure(true, true));
		assertEquals("Fujitsu", Constants.IO_VB_FUJITSU, m.getConverter4code(ICopybookDialects.FMT_FUJITSU).getFileStructure(true, true));
	}

}
