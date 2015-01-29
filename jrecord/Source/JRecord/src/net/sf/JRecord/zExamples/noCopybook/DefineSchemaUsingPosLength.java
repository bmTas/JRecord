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
public class DefineSchemaUsingPosLength {

	public DefineSchemaUsingPosLength() {
		try {
			ExternalRecord r = ExternalRecord
					.newFixedWidthRecord("My_Record", Constants.IO_TEXT_LINE, "")
						.addField("Sku"  , Type.ftChar             ,  1, 8, 0)
						.addField("Store", Type.ftNumRightJustified,  9, 3, 0)
						.addField("Date" , Type.ftNumRightJustified, 12, 6, 0)
						.addField("Dept" , Type.ftNumRightJustified, 18, 3, 0)
						.addField("Qty"  , Type.ftNumRightJustified, 21, 2, 0)
						.addField("Price", Type.ftNumRightJustified, 23, 6, 2)
					.asExternalRecord(); 
							
			LayoutDetail schema = r.asLayoutDetail();
			AbstractLine saleRecord;
			AbstractLineReader reader = LineIOProvider.getInstance().getLineReader(schema);
			
			reader.open(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile(), schema);
			
            while ((saleRecord = reader.read()) != null) {
                System.out.println(saleRecord.getFieldValue("Sku").asString()
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
		new DefineSchemaUsingPosLength();
	}
}
