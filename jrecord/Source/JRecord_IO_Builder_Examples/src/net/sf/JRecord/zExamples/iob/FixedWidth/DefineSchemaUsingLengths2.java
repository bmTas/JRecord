package net.sf.JRecord.zExamples.iob.FixedWidth;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Types.Type;


/**
 * This class defines a Schema using Field Lengths and letting JRecord calculate positions
 * 
 * @author Bruce Martin
 *
 */
public class DefineSchemaUsingLengths2 {

	public DefineSchemaUsingLengths2() {
		try {
			AbstractLineReader reader = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
									.setFileOrganization(Constants.IO_FIXED_LENGTH)
									.setFont("CP037")
									.defineFieldsByLength()
										.addFieldByLength("Sku"  , Type.ftChar,   8, 0)
										.addFieldByLength("Store", Type.ftPackedDecimal, 2, 0)
										.addFieldByLength("Date" , Type.ftPackedDecimal, 4, 0)
										.addFieldByLength("Dept" , Type.ftPackedDecimal, 2, 0)
										.addFieldByLength("Qty"  , Type.ftPackedDecimal, 5, 0)
										.addFieldByLength("Price", Type.ftPackedDecimal, 6, 2)
									.endOfRecord()
									.newReader(this.getClass().getResource("DTAR020.bin").getFile());
			AbstractLine saleRecord;
			
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
		new DefineSchemaUsingLengths2();
	}
}
