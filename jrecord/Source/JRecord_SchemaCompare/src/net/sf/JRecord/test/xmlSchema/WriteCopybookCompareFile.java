package net.sf.JRecord.test.xmlSchema;

import java.io.IOException;

import net.sf.JRecord.test.schema.cobol.io.WriteXmlSchemaDetails;

/**
 * This program reads through a directory of Cobol Copybooks
 * and 'Writes' the details to a 'Layout' File.
 * The values in the 'Layout' File are used by the {@link DoCompare}
 * program to compare with a recently generated 'Layout'
 *  
 * @author Bruce Martin
 *
 */
public class WriteCopybookCompareFile {

	public static void main(String[] args) throws IOException {
		ParmDetails pd = new ParmDetails(false, args);
		
		if (pd.isOk()) {
			new WriteXmlSchemaDetails(pd);
		}
	}
}

