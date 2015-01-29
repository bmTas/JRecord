package net.sf.JRecord.zExamples.other;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.Convert;

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
					Convert.FMT_OPEN_COBOL, 0, new TextLog());

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
