package net.sf.JRecord.cbl2csv.imp;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.cbl2csv.args.IUpdateFieldName;

/**
 * Multi-Record Cobol Data file to Csv
 * 
 * @author Bruce Martin
 *
 */
public class Cobol2CsvMr {


	private final String sep ;
	private final String quote;
	private final StringBuilder b;
	private final boolean writeRecordName;
	private final LayoutDetail schema;


	public Cobol2CsvMr(ICobolToCsvDefinition options) throws IOException {
		List<RecordWriterDetails> recordList = options.getRecordList();
		BufferedWriter csvWriter = options.getCsvWriter();
		this.schema = options.getSchema();
		
		boolean isReportInvalidFields = options.isReportInvalidFields();
		String  lowValueTxt = options.getLowValueTxt();
		String  highValueTxt = options.getHighValueTxt();
		String  numSpacesTxt = options.getNumericSpacesTxt();
		
		sep = options.getSeparator();
		quote = options.getQuote();
		
		
		WriterDetails[] recordWriters = new WriterDetails[schema.getRecordCount()];
		WriterDetails defaultWriter = null;
		
		writeRecordName = options.isWriteRecordName();
		if (csvWriter != null) {
			defaultWriter = new WriterDetails(schema.getRecord(0), csvWriter, options);
			Arrays.fill(recordWriters, defaultWriter);
		}
		
		for (RecordWriterDetails rec : recordList) {
			int idx = schema.getRecordIndex(rec.recordName);
			if (idx < 0) {
				System.err.println("Record: " + rec.recordName + " was not found in the schema");
			} else {
				recordWriters[idx] = new WriterDetails(schema.getRecord(idx), rec.writer, options);
			}
		}

	
		// Read the input file and write the Csv
		AbstractLine l;
		AbstractLineReader reader = options.getLineReader();
		int recordIdx, lineNumber = 0;
		b = new StringBuilder(Math.min(200, schema.getMaximumRecordLength() + 30));
		int[] recordFieldCounts = new int[schema.getRecordCount()];
		for (int i = 0; i < recordFieldCounts.length; i++) {
			recordFieldCounts[i] = schema.getRecord(i).getFieldCount();
			if ("filler".equalsIgnoreCase(schema.getField(i, recordFieldCounts[i]-1).getName())) {
				recordFieldCounts[i] -= 1;
			}
		}
		
		if (isReportInvalidFields) {
			while ((l = reader.read()) != null) {
				lineNumber += 1;
				recordIdx = l.getPreferredLayoutIdx();
				
				if (recordIdx < 0) {
					String lStr = l.getFullLine();
					System.err.println("Line: " + lineNumber + " Unkown Record Type " 
							 + (lStr.length() < 40 ? lStr : lStr.substring(0, 40)));
				} else {
					WriterDetails wd = recordIdx >= recordWriters.length ? defaultWriter : recordWriters[recordIdx];
				
					if (wd != null) {
						RecordDetail record = schema.getRecord(recordIdx);	
						String fieldSep = startRecord(recordIdx);
						
						for (int i = 0; i < recordFieldCounts[recordIdx]; i++) {
							IFieldValue field = l.getFieldValue(record.getField(i));
							String s="";
							
							b.append(fieldSep);
							boolean checkInvalid = TypeManager.isPackedDecimal(field.getFieldDetail().getType()) || ! field.isBinary();
							if (checkInvalid && field.isLowValues()) {
								s = lowValueTxt;
							} else if (checkInvalid && field.isHighValues()) {
								s = highValueTxt;
							} else if (checkInvalid && field.isNumeric() && field.isSpaces()) {
								s = numSpacesTxt;
							} else {
								try {
									s = field.asString();
								} catch (Exception e) {
									System.err.println("Line: " + lineNumber + " " + e);
								}
							}
							addField(s);
							fieldSep = sep;	
						}
						wd.writeLine(b.toString());
					}
				}
			}
		} else {
			while ((l = reader.read()) != null) {
				lineNumber += 1;
				recordIdx = l.getPreferredLayoutIdx();
			
				if (recordIdx < 0) {
					String lStr = l.getFullLine();
					System.err.println("Line: " + lineNumber + " Unkown Record Type " 
							+ " " + (lStr.length() < 40 ? lStr : lStr.substring(0, 40)));
				} else {
					WriterDetails wd = recordIdx >= recordWriters.length ? defaultWriter : recordWriters[recordIdx];
					if (wd != null) {
						//FieldIterator fieldIterator = l.getFieldIterator(recordIdx);
						RecordDetail record = schema.getRecord(recordIdx);	
		
						String fieldSep = startRecord(recordIdx);
						
						for (int i = 0; i < recordFieldCounts[recordIdx]; i++) {
							IFieldValue field = l.getFieldValue(record.getField(i));
							String s = "";
							try {
								s = field.asString();
							} catch (Exception e) {
								System.err.println("Line: " + lineNumber + " " + e);
							}
							
							b.append(fieldSep);
							addField(s);
							fieldSep = sep;	
						}
						wd.writeLine(b.toString());
					}
				}
			}
		}
		if (defaultWriter != null) {
			defaultWriter.close();
		}
		
		for (WriterDetails wd : recordWriters) {
			if (wd != null) {
				wd.close();
			}
		}
	}

	private String startRecord(int recordIdx) {
		String fieldSep = "";
		b.setLength(0);
		
		if (writeRecordName) {
			if (recordIdx >= 0 && recordIdx < schema.getRecordCount()) {
				b.append(schema.getRecord(recordIdx).getRecordName());
			}
			fieldSep = sep;	
		}
		return fieldSep;
	}

	private void addField(String s) {
		boolean addQuotes = (quote.length() > 0 && s.indexOf('\n') > 0 || s.indexOf(sep) > 0
				|| s.startsWith(quote));
		if (addQuotes) {
			b.append(quote);
			for (int i = 0; i < s.length(); i++) {
				if (s.startsWith(quote, i)) {
					b.append(quote).append(quote);
					i += quote.length() - 1;
				} else {
					b.append(s.charAt(i));
				}
			}
			b.append(quote);
		} else {
			b.append(s);
		}
	}
	
	/**
	 * Class to write one Csv file. The record can produce
	 * 
	 * 1) One Output Csv file
	 * 2) One Csv file per record type.
	 * 3) One Default Csv file a file for selected record-types
	 * 
	 * @author Bruce Martin
	 *
	 */
	private static class WriterDetails {
		RecordDetail recordDef;
		final BufferedWriter writer;
		final String separator;
		final boolean addRecordName;
		final IUpdateFieldName updateNames;
		boolean open = true;
		
		
		public WriterDetails(RecordDetail recordDef, BufferedWriter writer, ICobolToCsvDefinition options) {
			super();
			this.recordDef =  options.isCsvHeader() ? recordDef : null;
			this.writer = writer;
			this.addRecordName = options.isWriteRecordName();
			this.separator =options.getSeparator();
			this.updateNames = options.getUpdateFieldName();
		}

		/**
		 * @return the writer
		 * @throws IOException 
		 */
		public final void writeLine(String s) throws IOException {
			if (recordDef != null) {
				String sep = "";
				if (addRecordName) {
					writer.write(updateNames.updateName("Record-Name"));
					sep = separator;
				}
				
				for (int i = 0; i < recordDef.getFieldCount(); i++) {
					writer.write(sep + updateNames.updateName(recordDef.getField(i).getName()));
					sep = separator;
				}
				recordDef = null;
				writer.newLine();
			}
			writer.write(s);
			writer.newLine();
		}
		
		public final void close() throws IOException {
			if (open) {
				writer.close();
				open = false;
			}
		}
	}
}
