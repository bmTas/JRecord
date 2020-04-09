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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.detailsSelection.GetValue;

public class TstGroupFunctions extends TestCase {
	private String[] csvLines = {
//			"KEYCODE-NO	STORE-NO	DATE	DEPT-NO	QTY-SOLD	SALE-PRICE",
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
    public static final String TAB_CSV_LAYOUT =
    		   "<?xml version=\"1.0\" ?>"
			 + "<RECORD RECORDNAME=\"csvDTAR020\" COPYBOOK=\"csvDTAR020\" DELIMITER=\"&lt;tab&gt;\" FILESTRUCTURE=\"54\" STYLE=\"0\" RECORDTYPE=\"GroupOfRecords\" LIST=\"Y\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">"
			 + "	<RECORDS>"
			 + "		<RECORD RECORDNAME=\"GeneratedCsvRecord\" COPYBOOK=\"csvDTAR020_GeneratedCsvRecord\" DELIMITER=\"&lt;tab&gt;\" FILESTRUCTURE=\"54\" STYLE=\"0\" RECORDTYPE=\"Delimited\" LIST=\"N\" QUOTE=\"\" RecSep=\"default\" LINE_NO_FIELD_NAMES=\"1\">"
			 + "			<FIELDS>"
			 + "				<FIELD NAME=\"keycode-no\" DESCRIPTION=\"keycode-no\" POSITION=\"1\" TYPE=\"Char\"/>"
			 + "				<FIELD NAME=\"Store-No\" DESCRIPTION=\"Store-No\" POSITION=\"2\" TYPE=\"Char\"/>"
			 + "				<FIELD NAME=\"Date\" DESCRIPTION=\"Date\" POSITION=\"3\" TYPE=\"Char\"/>"
			 + "				<FIELD NAME=\"Dept-No\" DESCRIPTION=\"Dept-No\" POSITION=\"4\" TYPE=\"Char\"/>"
			 + "				<FIELD NAME=\"Qty-Sold\" DESCRIPTION=\"Qty-Sold\" POSITION=\"5\" TYPE=\"Char\"/>"
			 + "				<FIELD NAME=\"Sale-Price\" DESCRIPTION=\"Sale-Price\" POSITION=\"6\" TYPE=\"Char\"/>"
			 + "			</FIELDS>"
			 + "		</RECORD>"
			 + "	</RECORDS>"
			 + "</RECORD>";


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


    public void testNumeric() throws RecordException, Exception {
    	LayoutDetail l = getNumLayout();
    	List<Line> lines = getLines(l);

       	GetValue.Max max1 = new GetValue.Max(l.getRecord(0).getField(0), 0);
       	GetValue.Max max2 = new GetValue.Max(l.getRecord(0).getField(5), 0);
       	GetValue.Min min1 = new GetValue.Min(l.getRecord(0).getField(0), 0);
       	GetValue.Min min2 = new GetValue.Min(l.getRecord(0).getField(5), 0);
      	GetValue.First first1 = new GetValue.First(l.getRecord(0).getField(0), 0);
       	GetValue.First first2 = new GetValue.First(l.getRecord(0).getField(5), 0);
       	GetValue.Last last1 = new GetValue.Last(l.getRecord(0).getField(0), 0);
       	GetValue.Last last2 = new GetValue.Last(l.getRecord(0).getField(5), 0);
       	GetValue.Sum sum1 = new GetValue.Sum(l.getRecord(0).getField(0), 0, false);
       	GetValue.Sum sum2 = new GetValue.Sum(l.getRecord(0).getField(5), 0, false);
       	GetValue.Sum sum3 = new GetValue.Sum(l.getRecord(0).getField(4), 0, false);
       	GetValue.Sum ave1 = new GetValue.Sum(l.getRecord(0).getField(0), 0, true);
       	GetValue.Sum ave2 = new GetValue.Sum(l.getRecord(0).getField(5), 0, true);
       	GetValue.Sum ave3 = new GetValue.Sum(l.getRecord(0).getField(4), 0, true);

       	System.out.println(max1.getValue(lines) + " " + max2.getValue(lines)
       			+ " " + min1.getValue(lines) + " " + min2.getValue(lines)
    			+ " >> " + sum1.getValue(lines) + " " + sum2.getValue(lines) + " " + sum3.getValue(lines)
    			+ " >> " + ave1.getValue(lines) + " " + ave2.getValue(lines) + " " + ave3.getValue(lines)
       			);

       	assertEquals(new BigDecimal("69694158"), max1.getValue(lines));
       	assertEquals(new BigDecimal("69.99"), max2.getValue(lines));
       	assertEquals(new BigDecimal("60604100"), min1.getValue(lines));
       	assertEquals(new BigDecimal("-69.99"), min2.getValue(lines));
       	assertEquals(("63604808"), first1.getValue(lines));
       	assertEquals(("4.87"), first2.getValue(lines));
       	assertEquals(("68674560"), last1.getValue(lines));
       	assertEquals(("5.99"), last2.getValue(lines));

       	assertEquals(new BigDecimal("1101217407"), sum1.getValue(lines));
       	assertEquals(new BigDecimal("88.35"), sum2.getValue(lines));
       	assertEquals(new BigDecimal("11"), sum3.getValue(lines));

       	assertEquals(new BigDecimal("64777494.5294117648"), ave1.getValue(lines));
       	assertEquals(new BigDecimal("5.1970588236"), ave2.getValue(lines));
       	assertEquals(new BigDecimal("0.6470588236"), ave3.getValue(lines));
  }


    public void testText() throws RecordException, Exception {
    	LayoutDetail l = getLayout();
    	List<Line> lines = getLines(l);

       	GetValue.Max max1 = new GetValue.Max(l.getRecord(0).getField(0), 0);
       	GetValue.Max max2 = new GetValue.Max(l.getRecord(0).getField(5), 0);
       	GetValue.Min min1 = new GetValue.Min(l.getRecord(0).getField(0), 0);
       	GetValue.Min min2 = new GetValue.Min(l.getRecord(0).getField(5), 0);
       	GetValue.First first1 = new GetValue.First(l.getRecord(0).getField(0), 0);
       	GetValue.First first2 = new GetValue.First(l.getRecord(0).getField(5), 0);
       	GetValue.Last last1 = new GetValue.Last(l.getRecord(0).getField(0), 0);
       	GetValue.Last last2 = new GetValue.Last(l.getRecord(0).getField(5), 0);

       	System.out.println(max1.getValue(lines) + " " + max2.getValue(lines)
			+ " " + min1.getValue(lines) + " " + min2.getValue(lines)
			);


       	assertEquals("69694158", max1.getValue(lines));
       	assertEquals("8.99", max2.getValue(lines));
       	assertEquals(("60604100"), min1.getValue(lines));
       	assertEquals(("-17.99"), min2.getValue(lines));
       	assertEquals(("63604808"), first1.getValue(lines));
       	assertEquals(("4.87"), first2.getValue(lines));
       	assertEquals(("68674560"), last1.getValue(lines));
       	assertEquals(("5.99"), last2.getValue(lines));
    }

    private List<Line> getLines(LayoutDetail l) {
    	ArrayList<Line> lines = new ArrayList<Line>(csvLines.length);

    	for (String s : csvLines) {
    		lines.add(new Line(l, s));
    	}

    	return lines;
    }

	private LayoutDetail getLayout() throws RecordException, Exception {
		return getExternalLayout(TAB_CSV_LAYOUT).asLayoutDetail();
	}



	private LayoutDetail getNumLayout() throws RecordException, Exception {
		return getExternalLayout(TAB_CSV_LAYOUT_NUM).asLayoutDetail();
	}

	private ExternalRecord getExternalLayout(String strLayout) throws Exception {
		return RecordEditorXmlLoader.getExternalRecord(strLayout, "Csv Layout");
	}

}
