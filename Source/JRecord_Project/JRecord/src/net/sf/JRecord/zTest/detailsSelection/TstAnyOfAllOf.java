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

package net.sf.JRecord.zTest.detailsSelection;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.detailsSelection.FieldSelect;
import net.sf.JRecord.detailsSelection.FieldSelectX;

public class TstAnyOfAllOf extends TestCase {

	private String[] csvLines = {
			"63604808	20	40118	170	1	4.87",
			"69684558	20	40118	280	1	19.00",
			"69684558	20	40118	280	-1	-19.00",
			"69694158	20	40118	280	1	5.01",
			"62684671	20	-40118	685	1	69.99",
			"62684671	20	40118	685	-1	-69.99",
			"61664713	59	40118	335	1	17.99",
			"61664713	59	40118	335	-1	-17.99",
			"61684613	59	40118	335	1	12.99",
			"68634752	59	40118	410	1	8.99",
			"60694698	59	40118	620	1	3.99",
			"60664659	59	40118	620	1	3.99",
			"60614487	59	40118	878	1	5.95",
			"68654655	166	-40118	60	1	5.08",
			"69624033	166	40118	80	1	18.19",
			"60604100	166	40118	80	1	13.30",
			"68674560	166	40118	170	1	5.99",
	};


    public static final String TAB_CSV_LAYOUT_NUM =
    		  "<?xml version=\"1.0\" ?>"
			 + "<RECORD RECORDNAME=\"csvNumDTAR020\" COPYBOOK=\"csvNumDTAR020\" DELIMITER=\"&lt;tab&gt;\" FILESTRUCTURE=\"54\" STYLE=\"0\" RECORDTYPE=\"GroupOfRecords\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">"
			 + "	<RECORDS>"
			 + "		<RECORD RECORDNAME=\"GeneratedCsvRecord\" COPYBOOK=\"csvNumDTAR020_GeneratedCsvRecord\" DELIMITER=\"&lt;tab&gt;\" FILESTRUCTURE=\"54\" STYLE=\"0\" RECORDTYPE=\"Delimited\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">"
			 + "			<FIELDS>"
			 + "				<FIELD NAME=\"keycode-no\" DESCRIPTION=\"keycode-no\" POSITION=\"1\" TYPE=\"NumAnyDecimal\"/>"
			 + "				<FIELD NAME=\"Store-No\" DESCRIPTION=\"Store-No\" POSITION=\"2\" TYPE=\"NumAnyDecimal\"/>"
			 + "				<FIELD NAME=\"Date\" DESCRIPTION=\"Date\" POSITION=\"3\" TYPE=\"NumAnyDecimal\"/>"
			 + "				<FIELD NAME=\"Dept-No\" DESCRIPTION=\"Dept-No\" POSITION=\"4\" TYPE=\"NumAnyDecimal\"/>"
			 + "				<FIELD NAME=\"Qty-Sold\" DESCRIPTION=\"Qty-Sold\" POSITION=\"5\" TYPE=\"NumAnyDecimal\"/>"
			 + "				<FIELD NAME=\"Sale-Price\" DESCRIPTION=\"Sale-Price\" POSITION=\"6\" DECIMAL=\"2\" TYPE=\"Num (Left Justified)\"/>"
			 + "			</FIELDS>"
			 + "		</RECORD>"
			 + "	</RECORDS>"
			 + "</RECORD>";

	private List<Line> lines = getLines();
	private static final FieldSelect yes = FieldSelectX.getTrueSelection();
	private static final FieldSelect no = new FieldSelect("", "", "", null) {

		@Override
		public boolean isSelected(Object line) {
			return false;
		}
	};
	private final FieldSelect odd = new FieldSelect("", "", "", null) {
		int idx = 0;
		@Override
		public boolean isSelected(Object line) {

			return (idx++) % 2 == 1;
		}
	};

	private final FieldSelect first = new FieldSelect("", "", "", null) {
		int idx = 0;
		@Override
		public boolean isSelected(Object line) {

			return (idx++)  == 0;
		}
	};



    public void testAnyOf() {
    	FieldSelectX.AnyAllOf anyAllOf = new FieldSelectX.AnyAllOf(yes,   true);
		assertTrue( "Any Of - yes",   anyAllOf.isSelected(lines));
    	assertTrue( "Any Of - odd",   (new FieldSelectX.AnyAllOf(odd,   true)).isSelected(lines));
    	assertTrue( "Any Of - first", (new FieldSelectX.AnyAllOf(first, true)).isSelected(lines));
    	assertFalse("Any Of - no",    (new FieldSelectX.AnyAllOf(no,    true)).isSelected(lines));
    }


    public void testAllOf() {
    	assertTrue( "Any Of - yes",   (new FieldSelectX.AnyAllOf(yes,   false)).isSelected(lines));
    	assertFalse("Any Of - odd",   (new FieldSelectX.AnyAllOf(odd,   false)).isSelected(lines));
    	assertFalse("Any Of - first", (new FieldSelectX.AnyAllOf(first, false)).isSelected(lines));
    	assertFalse("Any Of - no",    (new FieldSelectX.AnyAllOf(no,    false)).isSelected(lines));
    }

    private List<Line> getLines() {
    	try {
	    	LayoutDetail l = getNumLayout();
	    	ArrayList<Line> lines = new ArrayList<Line>(csvLines.length);

	    	for (String s : csvLines) {
	    		lines.add(new Line(l, s));
	    	}

	    	return lines;
    	} catch (Exception e) {
			e.printStackTrace();
		}
    	return null;
    }

	private LayoutDetail getNumLayout() throws RecordException, Exception {
		return getExternalLayout(TAB_CSV_LAYOUT_NUM).asLayoutDetail();
	}

	private ExternalRecord getExternalLayout(String strLayout) throws Exception {
		return RecordEditorXmlLoader.getExternalRecord(strLayout, "Csv Layout");
	}

}
