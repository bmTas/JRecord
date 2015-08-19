package net.sf.JRecord.zExamples.iob.FixedWidth;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Types.Type;


/**
 * This class defines a Schema using Field positions and letting JRecord calculate lengths
 * 
 * @author Bruce Martin
 *
 */
public class DefineSchemaUsingPosition {

	public DefineSchemaUsingPosition() {
		try {
			AbstractLineReader reader = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
						.defineFieldsByPosition()
							.addFieldByPosition("Sku"  , Type.ftChar             ,  1, 0)
							.addFieldByPosition("Store", Type.ftNumRightJustified,  9, 0)
							.addFieldByPosition("Date" , Type.ftNumRightJustified, 12, 0)
							.addFieldByPosition("Dept" , Type.ftNumRightJustified, 18, 0)
							.addFieldByPosition("Qty"  , Type.ftNumRightJustified, 21, 0)
							.addFieldByPosition("Price", Type.ftNumRightJustified, 23, 2)
						.endOfRecord(29)
						.newReader(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile());
			AbstractLine saleRecord;
			
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
		new DefineSchemaUsingPosition();
	}
}
