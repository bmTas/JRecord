package net.sf.JRecord.zExamples.iob.csv;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.FieldIterator;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Copy a Fixed width file to a Csv
 * The Fixed-Width files and Csv files are defined with IOBuilders.
 * 
 * @author Bruce Martin
 *
 */
public class WriteCsvFile {

	public WriteCsvFile() {
		
		CommonBits.setUseCsvLine(true);
		try {
			IFixedWidthIOBuilder inIOBuilder = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
						.defineFieldsByLength()
							.addFieldByLength("Sku"  , Type.ftChar,              8, 0)
							.addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
							.addFieldByLength("Date" , Type.ftNumRightJustified, 6, 0)
							.addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
							.addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
							.addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
						.endOfRecord();

			ICsvIOBuilder outIOBuilder = JRecordInterface1.CSV.newIOBuilder(",", "'")
						.defineFields()
							.addCsvField("Sku",   Type.ftChar, 0)
							.addCsvField("Store", Type.ftNumAnyDecimal, 0)
							.addCsvField("Date",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Dept",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Qty",   Type.ftNumAnyDecimal, 0)
							.addCsvField("Price", Type.ftNumAnyDecimal, 0)
						.endOfRecord();


			String outputFileName = TstConstants.TEMP_DIRECTORY + "csvDTAR020_Tst1.csv";
			AbstractLine saleRecord;
			AbstractLine outCsvRecord = new CharLine(outIOBuilder.getLayout(), "");
			AbstractLineReader reader = inIOBuilder.newReader(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile());
			AbstractLineWriter writer = outIOBuilder.newWriter(outputFileName);

			System.out.println("Output File: " + outputFileName);
			try {
				while ((saleRecord = reader.read()) != null) {
					 FieldIterator fields = saleRecord.getFieldIterator(0);
					 
					 for (AbstractFieldValue fieldValue : fields) {
						 outCsvRecord.getFieldValue(fieldValue.getFieldDetail().getName())
						 			 .set(fieldValue.asString());
					 }
					 writer.write(outCsvRecord);
				}
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				reader.close();
				writer.close();
			}
		} catch (RecordException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args) {
		new WriteCsvFile();
	}
}
