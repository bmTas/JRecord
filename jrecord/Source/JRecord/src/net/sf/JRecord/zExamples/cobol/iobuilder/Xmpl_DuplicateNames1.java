package net.sf.JRecord.zExamples.cobol.iobuilder;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


/**
 * This example illustrates using CobolIOBuilder to create the reader and the 
 * getGroupField method to retrieve duplicate fields
 * This method retrieves a Field Definition using both the Field-Name and Group-Names.
 * You only need specify enough "Group-Names" to uniquely identify a field but you do
 * not need to specify all of them. The Group names can be specified in any sequence.
 *
 *
 * @author Bruce Martin
 *
 */
public class Xmpl_DuplicateNames1 {

	// In this cobol copybook, FIRST-NAME and LAST-NAME appear 3 times
	// to uniquely identify the field, we need specify both Group and field names.
	private static String cobolCopybook
			= "      01 COMPANY-RECORD.\n"
			+ "         05 COMPANY-NAME     PIC X(30).\n"
			+ "         05 EMPLOYEE-LIST.\n"
			+ "            10 PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 VICE-PRESIDENT.\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "            10 OTHERS.\n"
			+ "               15 TITLE      PIC X(10).\n"
			+ "               15 LAST-NAME  PIC X(15).\n"
			+ "               15 FIRST-NAME PIC X(8).\n"
			+ "         05 ADDRESS          PIC X(15).\n"
			+ "         05 CITY             PIC X(15).\n"
			+ "         05 STATE            PIC XX.\n"
			+ "         05 ZIP              PIC 9(5).\n";

	private static ByteArrayInputStream cobolCopybookStream = new ByteArrayInputStream(cobolCopybook.getBytes());
	private static String dataFile
				= "BM                            Martin         Bruce   XX             YY      Mr        Aa             Bb      Me             George Town    Ta07253\n"
				+ "JPY                           John           Young   Young          George  Mr        Hary           Vander  123            456            1100111\n"
				+ "Fleetwood Mac                 Fleetwood      Mick    Stevie         Nicks   Ms        McVie          Christinx              y              z 01234\n"
				;

	public static void main(String[] args) throws RecordException, IOException {
			
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
										.newIOBuilder(cobolCopybookStream, "COMPANY-RECORD")
											.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE); 
												// Setting the FileOrganisation is Not needed in this case 
												// but it is a good idea to set the 
												// file organization (structure) rather than 
		                                        // letting JRecord try and work it out.
		
		AbstractLineReader reader  = ioBuilder.newReader(new ByteArrayInputStream(dataFile.getBytes()));
		LayoutDetail schema = ioBuilder.getLayout();


			// ** Retrieve the Field definitions from the RecordDefinition **
			// ** -------------------------------------------------------- **
		    //
		    //  The copybook has duplicate field names so we must use getGroupField
		    //  and supply enough "Groups" to uniquely identify each field.
		IFieldDetail presidentFirstNameFld     = schema.getGroupField("PRESIDENT", "FIRST-NAME");
		IFieldDetail vicePresidentFirstNameFld = schema.getGroupField("VICE-PRESIDENT", "FIRST-NAME");
		IFieldDetail otherFirstNameFld         = schema.getGroupField("OTHERS", "FIRST-NAME");

		IFieldDetail presidentLastNameFld      = schema.getGroupField("PRESIDENT", "LAST-NAME");
		IFieldDetail vicePresidentLastNameFld  = schema.getGroupField("VICE-PRESIDENT", "LAST-NAME");
		IFieldDetail otherLastNameFld          = schema.getGroupField("OTHERS", "LAST-NAME");


			//Get the company-name field definition.
		IFieldDetail companyNameFld = schema.getFieldFromName("COMPANY-NAME");

		AbstractLine line;

		while ((line = reader.read()) != null) {
			System.out.println(
					  line.getFieldValue(companyNameFld)           .asString() + "\t"
					+ line.getFieldValue(presidentFirstNameFld)    .asString() + "\t"
					+ line.getFieldValue(presidentLastNameFld)     .asString() + "\t|\t"
					+ line.getFieldValue(vicePresidentFirstNameFld).asString() + "\t"
					+ line.getFieldValue(vicePresidentLastNameFld) .asString() + "\t|\t"
					+ line.getFieldValue(otherFirstNameFld)        .asString() + "\t"
					+ line.getFieldValue(otherLastNameFld)         .asString()
			);
		}

		reader.close();
	}
}
