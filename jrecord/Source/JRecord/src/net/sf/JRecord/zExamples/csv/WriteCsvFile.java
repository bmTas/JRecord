package net.sf.JRecord.zExamples.csv;

import java.io.IOException;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.FieldIterator;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Copy a Fixed width file to a Csv
 * The Fixed-Width files and Csv files are defined with the Helper methods.
 * 
 * @author Bruce Martin
 *
 */
public class WriteCsvFile {

	public WriteCsvFile() {
		
		CommonBits.setUseCsvLine(true);
		try {
			LayoutDetail inSchema = ExternalRecord
						.newFixedWidthRecord("My_Record", Constants.IO_TEXT_LINE, "")
							.addFieldByLength("Sku"  , Type.ftChar,              8, 0)
							.addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
							.addFieldByLength("Date" , Type.ftNumRightJustified, 6, 0)
							.addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
							.addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
							.addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
						.asLayoutDetail(); 
			
			   // For output Csv files you need to define the fields
			LayoutDetail outSchema = ExternalRecord
						.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", ";", "\"")
							.addCsvField("Sku",   Type.ftChar, 0)
							.addCsvField("Store", Type.ftNumAnyDecimal, 0)
							.addCsvField("Date",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Dept",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Qty",   Type.ftNumAnyDecimal, 0)
							.addCsvField("Price", Type.ftNumAnyDecimal, 0)
						.asLayoutDetail();

			AbstractLine saleRecord;
			AbstractLine outCsvRecord = new CharLine(outSchema, "");
			LineIOProvider ioProvider = LineIOProvider.getInstance();
			AbstractLineReader reader = ioProvider.getLineReader(inSchema);
			AbstractLineWriter writer = ioProvider.getLineWriter(outSchema);
			String outputFileName = TstConstants.TEMP_DIRECTORY + "csvDTAR020_Tst1.csv";

			System.out.println("Output File: " + outputFileName);
			try {
				reader.open(this.getClass().getResource("DTAR020_tst1.bin.txt").getFile(), inSchema);
				writer.open(outputFileName);

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
