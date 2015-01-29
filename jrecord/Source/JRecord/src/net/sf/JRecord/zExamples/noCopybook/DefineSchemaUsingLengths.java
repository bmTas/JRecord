package net.sf.JRecord.zExamples.noCopybook;

import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;


/**
 * This class defines a Schema using Field Lengths and letting JRecord calculate positions
 * 
 * @author Bruce Martin
 *
 */
public class DefineSchemaUsingLengths {

	public DefineSchemaUsingLengths() {
		try {
			LayoutDetail schema = ExternalRecord
									.newFixedWidthRecord("My_Record", Constants.IO_TEXT_LINE, "")
										.addFieldByLength("Sku"  , Type.ftChar,   8, 0)
										.addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
										.addFieldByLength("Date" , Type.ftNumRightJustified, 6, 0)
										.addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
										.addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
										.addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
									.asLayoutDetail();
			AbstractLine saleRecord;
			AbstractLineReader reader = LineIOProvider.getInstance().getLineReader(schema);
			
			reader.open(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile(), schema);
			
            while ((saleRecord = reader.read()) != null) {
                System.out.println(
                				 saleRecord.getFieldValue("Sku").asString()
                        + "\t" + saleRecord.getFieldValue("Store").asString()
                        + "\t" + saleRecord.getFieldValue("Date").asString()
                        + "\t" + saleRecord.getFieldValue("Dept").asString()
                        + "\t" + saleRecord.getFieldValue("Qty").asString()
                        + "\t" + saleRecord.getFieldValue("Price").asString());

            }
            reader.close();
		} catch (RecordException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		new DefineSchemaUsingLengths();
	}
}
