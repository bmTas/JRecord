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
      
package net.sf.JRecord.zExamples.cobol.specialCases;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TestCommonCode;

/**
 * This example illustrates using getUniqueField method.
 * This method retrieves a Field Definition using both the Field-Name and Group-Names.
 * You only need specify enough "Group-Names" to uniquely identify a field but you do
 * not need to specify all of them. The Group names can be specified in any sequence.
 *
 * There is a second method getUniqueFieldGroupsInSequence where group-names must be
 * specified in the correct sequence. See Xmpl_DuplicateNames2 for an example
 * of using getUniqueFieldGroupsInSequence
 *
 * @author Bruce Martin
 *
 */
public class Xmpl_DuplicateNames1 {

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


	private static String dataFile
				= "BM                            Martin         Bruce   XX             YY      Mr        Aa             Bb      Me             George Town    Ta07253\n"
				+ "JPY                           John           Young   Young          George  Mr        Hary           Vander  123            456            1100111\n"
				+ "Fleetwood Mac                 Fleetwood      Mick    Stevie         Nicks   Ms        McVie          Christinx              y              z 01234\n"
				;

	public static void main(String[] args) throws RecordException, IOException {
			// Create an Internal JRecord schema (or layout) from the cobol copybook
		LayoutDetail schema = TestCommonCode.getLayoutFromCobolStr(
				cobolCopybook, "COMPANY-RECORD",
				CopybookLoader.SPLIT_NONE, "", ICopybookDialects.FMT_INTEL);
		int recordIdx = 0;  // since there is only one record type, the record index must be zero
		                    // If there where more than one record, you could use:
		                    //    schema.getRecordIndex("COMPANY-RECORD");

			// Retrieve the Record-Definition
		RecordDetail record = schema.getRecord(recordIdx);

			// ** Retrieve the Field definitions from the RecordDefinition **
			// ** -------------------------------------------------------- **
		IFieldDetail presidentFirstNameFld     = record.getGroupField("PRESIDENT", "FIRST-NAME");
		IFieldDetail vicePresidentFirstNameFld = record.getGroupField("VICE-PRESIDENT", "FIRST-NAME");
		IFieldDetail otherFirstNameFld         = record.getGroupField("OTHERS", "FIRST-NAME");

		IFieldDetail presidentLastNameFld      = record.getGroupField("PRESIDENT", "LAST-NAME");
		IFieldDetail vicePresidentLastNameFld  = record.getGroupField("VICE-PRESIDENT", "LAST-NAME");
		IFieldDetail otherLastNameFld          = record.getGroupField("OTHERS", "LAST-NAME");


			//Get the compane-name field definition.
		IFieldDetail companyNameFld = schema.getFieldFromName("COMPANY-NAME");

		AbstractLineReader reader  = LineIOProvider.getInstance().getLineReader(Constants.IO_BIN_TEXT);
		AbstractLine line;

		reader.open(new ByteArrayInputStream(dataFile.getBytes()), schema);

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
