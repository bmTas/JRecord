/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.other;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;

/**
 * This is not intended to be run, it shows you how to read a cobol copybook
 * and use it to create  Lines
 *
 * @author Bruce Martin
 *
 */
public class XmplBldLine01 {

	private XmplBldLine01() {
		try {
			String cobolString1 = ".............";
			String cobolString2 = ".............";

			/* Get the Cobol Copybook reader */
			CopybookLoader cpybookLoader = CopybookLoaderFactory.getInstance()
											.getLoader(CopybookLoaderFactory.COBOL_LOADER);

			/* Load as interchange format */
			ExternalRecord rec = cpybookLoader.loadCopyBook("CobolCopybook", CopybookLoader.SPLIT_NONE, 0,
					/* Font name */ "",
					ICopybookDialects.FMT_GNU_COBOL, 0, new TextLog());

			/* Create Layout / Description */
			LayoutDetail cobolDescription = rec.asLayoutDetail();

			/* Create Line */
			Line l = new Line(cobolDescription, cobolString1);


			/* Once a line is created, you can also do */

			l.setData(cobolString2);

		} catch (Exception e) {
			// TODO: handle exception
		}
	}

//    public static void main(String[] args) {
//        new XmplBldLine01();
//    }
}
