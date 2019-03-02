package net.sf.JRecord.test.schema;

import java.io.IOException;

import net.sf.JRecord.test.schema.cobol.io.CompareCopybookDetails;

/**
 * Batch Compare program to compare JRecord-Layouts
 * against the values stored in the Layout-File created
 * by {@link WriteCopybookCompareFile}
 * 
 * @author Bruce Martin
 *
 */
public class DoCompare {

	public static void main(String[] args) throws IOException {
		ParmDetails pd = new ParmDetails(true, args);
		
		if (pd.isOk()) {
			new CompareCopybookDetails(pd);
		}
	}
}
