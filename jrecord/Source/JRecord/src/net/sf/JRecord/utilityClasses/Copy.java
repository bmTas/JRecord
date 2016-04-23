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

package net.sf.JRecord.utilityClasses;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.IFieldValue;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.def.IO.builders.INewLineCreator;

/**
 * This is a Class containing static methods to read a file using a Schema and write the
 * data contents out with a different Schema. It could be used to copy <b>Cobol</b>
 * data files to <b>Csv</b> or Csv files to Cobol etc.
 * 
 * 
 * <p>At the moment (19-Oct-2014) this should be considered a Test module; 
 * so use it with <b>Caution</b>
 * 
 * 
 * @author Bruce Martin
 *
 */
public class Copy {

	/**
	 * This method copies an input file to an output file record by record.
	 * The fields on the input record are copies to the output record by matching
	 * the field names.
	 * <pre>
	 * i.e.
	 *    outputLine.field("Field-Name") = inputline.field("Field-Name")
	 * </pre>
	 * 
	 * <p>At the moment (19-Oct-2014) this should be considered a Test module; 
	 * so use it with <b>Caution</b>
	 * 
	 * @param inSchema input schema (layout or file description)
	 * @param inFileName input filename
	 * @param outSchema output schema (layout or file description)
	 * @param outFileName output filename
	 * 
	 * @throws IOException Any IO error
	 */
	public static void copyFileByMatchingFieldNames(
			LayoutDetail inSchema,  String inFileName,
			LayoutDetail outSchema, String outFileName)
	throws IOException {
		LineIOProvider ioProvider = LineIOProvider.getInstance();
		AbstractLineReader in = ioProvider.getLineReader(inSchema); 
		AbstractLineWriter out = ioProvider.getLineWriter(outSchema);
		int count = 0;
		

		in.open(inFileName, inSchema);
		out.open(outFileName);
		
		count = copyFileByMatchingFieldNames(in, out, outSchema);

		System.out.println("Copied " + count + " Records from " + inFileName + " to " + outFileName);
	}
	
	/**
	 * 
	 * @deprecated
	 */
	public static int copyFileByMatchingFieldNames(AbstractLineReader reader, AbstractLineWriter writer,  AbstractLine outLine) 
	throws IOException {
		return copyFileByMatchingFieldNames(reader, writer, outLine.getLayout());
	}
	
	/**
	 * This method copies an input file to an output file record by record.
	 * The fields on the input record are copies to the output record by matching
	 * the field names.
	 * <pre>
	 * i.e.
	 *    outputLine.field("Field-Name") = inputline.field("Field-Name")
	 * </pre>
	 * 
	 * <p>At the moment (19-Oct-2014) this should be considered a Test module; 
	 * so use it with <b>Caution</b>

	 * @param reader input reader source of input lines (or records)
	 * @param writer output writer
	 * @param outLine output line used to write to the file
	 * 
	 * @throws IOException any IO exception that occurs
	 */
	public static int copyFileByMatchingFieldNames(AbstractLineReader reader, AbstractLineWriter writer,  LayoutDetail outSchema) 
	throws IOException {

		AbstractLine inLine = reader.read();
		LayoutDetail inSchema = reader.getLayout();
		INewLineCreator lineCreator = newCreator(outSchema);
		
		if (inSchema.getRecordCount() > 1) {
			throw new RecordException("Only one Record Type is allowed on the Input RecordLayout");
		} 
		if (outSchema.getRecordCount() > 1) {
			throw new RecordException("Only one Record Type is allowed on the Output RecordLayout");
		}
		
		RecordDetail outRecordDef = outSchema.getRecord(0);
		RecordDetail inRecordDef  = inSchema .getRecord(0);
		int fieldCount = outRecordDef.getFieldCount();
		boolean missingFields = false;
		ArrayList<FieldDetail> fieldMapping = new ArrayList<FieldDetail>(fieldCount);

		

		for (int i = 0; i < fieldCount; i++) {
			fieldMapping.add(inRecordDef.getField(outRecordDef.getField(i).getLookupName()));
			if (fieldMapping.get(fieldMapping.size() - 1) == null) {
				System.out.println("No Field Match found for " + outRecordDef.getField(i).getLookupName());
				missingFields = true;
			}
		}
		
		if (missingFields) {
			System.out.println();
			System.out.println("Input File Fields: ");
			for (int i = 0; i < inRecordDef.getFieldCount(); i++) {
				System.out.println("\t" + inRecordDef.getField(i).getName());
			}
		}
		
		return doCopy(reader, writer, fieldMapping, inLine, lineCreator);
	}
	
	/**
	 * This method copies an input file to an output file record by record.
	 * The fields on the input record are copies to the output record by matching
	 * the field names.
	 * <pre>
	 * i.e.
	 *    outputLine.field("Field-Name") = inputline.field("Field-Name")
	 * </pre>
	 * 
	 * <p>At the moment (19-Oct-2014) this should be considered a Test module; 
	 * so use it with <b>Caution</b>

	 * @param reader input reader source of input lines (or records)
	 * @param writer output writer
	 * @param outLine output line used to write to the file
	 * 
	 * @throws IOException any IO exception that occurs
	 */
	public static int copyFileUsingMap(AbstractLineReader reader, AbstractLineWriter writer,  LayoutDetail outSchema,
			List<String> nameList, INewLineCreator lineCreator) 
	throws IOException {

		AbstractLine inLine = reader.read();
		LayoutDetail inSchema = reader.getLayout();
		
		if (lineCreator == null) {
			lineCreator = newCreator(outSchema);
		}
		
		if (inSchema.getRecordCount() > 1) {
			throw new RecordException("Only one Record Type is allowed on the Input RecordLayout");
		} 
		if (outSchema.getRecordCount() > 1) {
			throw new RecordException("Only one Record Type is allowed on the Output RecordLayout");
		}
		
		RecordDetail outRecordDef = outSchema.getRecord(0);
		RecordDetail inRecordDef  = inSchema .getRecord(0);
		int fieldCount = outRecordDef.getFieldCount();
		boolean missingFields = false;
		ArrayList<FieldDetail> fieldMapping = new ArrayList<FieldDetail>(fieldCount);

		

		for (int i = 0; i < fieldCount; i++) {
			fieldMapping.add(inRecordDef.getField(nameList.get(i)));
			//fieldMapping.add(map.get(outRecordDef.getField(i).getLookupName().toLowerCase()));
			if (fieldMapping.get(fieldMapping.size() - 1) == null) {
				System.out.println("No Field Match found for " + outRecordDef.getField(i).getLookupName());
				missingFields = true;
			}
		}
		
		if (missingFields) {
			System.out.println();
			System.out.println("Input File Fields: ");
			for (int i = 0; i < inRecordDef.getFieldCount(); i++) {
				System.out.println("\t" + inRecordDef.getField(i).getName());
			}
		}
		
		return doCopy(reader, writer, fieldMapping, inLine, lineCreator);
	}
	
	/**
	 * This method copies an input file to an output file record by record.
	 * The fields on the input record are copies to the output record by field number
	 * <pre>
	 * i.e.
	 *    outputLine.field(i) = inputline.field(i)
	 * </pre>
	 * 
	 * <p>At the moment (19-Oct-2014) this should be considered a Test module; 
	 * so use it with <b>Caution</
	 * 
	 * @param inSchema input schema (layout or file description)
	 * @param inFileName input filename
	 * @param outSchema output schema (layout or file description)
	 * @param outFileName output filename
	 * 
	 * @throws IOException Any IO error
	 */
	public static void copyFileByFieldNumber(
			LayoutDetail inSchema,  String inFileName,
			LayoutDetail outSchema, String outFileName)
	throws IOException {
		LineIOProvider ioProvider = LineIOProvider.getInstance();
		AbstractLineReader in = ioProvider.getLineReader(inSchema);
		AbstractLineWriter out = ioProvider.getLineWriter(outSchema);
		int count = 0;
		

		in.open(inFileName, inSchema);
		out.open(outFileName);
		
		count = copyFileByFieldNumber(in, out, outSchema);

		System.out.println("Copied " + count + " Records from " + inFileName + " to " + outFileName);
	}

	@Deprecated
	public static int copyFileByFieldNumber(AbstractLineReader reader, AbstractLineWriter writer,  AbstractLine outLine) 
	throws IOException {
		return copyFileByFieldNumber(reader, writer, outLine.getLayout());
	}

	/**
	 * copy the file (move the field number i 
	 * in the input record to field number i in the output record)
	 * <pre>
	 * i.e.
	 *    outputLine.field(i) = inputline.field(i)
	 * </pre>
	 * 
	 * @param reader input reader source of input lines (or records)
	 * @param writer output writer
	 * @param outLine output line used to write to the file
	 * 
	 * @return number of records copied
	 * 
	 * @throws IOException
	 * @throws RecordException
	 */
	public static int copyFileByFieldNumber(AbstractLineReader reader, AbstractLineWriter writer,  LayoutDetail outSchema) 
	throws IOException {
		
		AbstractLine inLine = reader.read();
		LayoutDetail inSchema = reader.getLayout();
		INewLineCreator lineCreator = newCreator(outSchema);
		
		if (inSchema.getRecordCount() > 1) {
			throw new RecordException("Only one Record Type is allowed on the Input RecordLayout");
		} 
		if (outSchema.getRecordCount() > 1) {
			throw new RecordException("Only one Record Type is allowed on the Output RecordLayout");
		}
		
		RecordDetail outRecordDef = outSchema.getRecord(0);
		RecordDetail inRecordDef  = inSchema .getRecord(0);
		int fieldCount = outRecordDef.getFieldCount();
		int min = Math.min(fieldCount, inRecordDef.getFieldCount());
	
		ArrayList<FieldDetail> fieldMapping = new ArrayList<FieldDetail>(fieldCount);

		
		for (int i = 0; i < min; i++) {
			fieldMapping.add(inRecordDef.getField(i));
		}
		
		for (int i = min; i < fieldCount; i++) {
			fieldMapping.add(null);
		}

		return doCopy(reader, writer, fieldMapping, inLine, lineCreator);
	}

	/**
	 * Do the actual file copy
	 * @param reader file reader to read the data from
	 * @param writer file writer to write the output data
	 * @param fieldMapping mapping between input record and output record
	 * @param inLine input line
	 * @param outLine output line that will be writtent to the file
	 * @return number of records written
	 * 
	 * @throws IOException any IO error (on the close)
	 */
	public static int doCopy(AbstractLineReader reader, AbstractLineWriter writer, 
			List<FieldDetail> fieldMapping,
			INewLineCreator lineCreator) throws IOException {
		return doCopy(reader, writer, fieldMapping, reader.read(), lineCreator);
		
	}

	/**
	 * Do the actual file copy
	 * @param reader file reader to read the data from
	 * @param writer file writer to write the output data
	 * @param fieldMapping mapping between input record and output record
	 * @param inLine input line
	 * @param outLine output line that will be writtent to the file
	 * @return number of records written
	 * 
	 * @throws IOException any IO error (on the close)
	 */
	private static int doCopy(AbstractLineReader reader, AbstractLineWriter writer, 
			List<FieldDetail> fieldMapping,
			AbstractLine inLine, INewLineCreator lineCreator) 
	throws IOException {
		
		int fldNum = 0;
		int lineNumber = 0;
		int fieldCount = fieldMapping.size();
		String v = "";
		try {			
			while (inLine != null) {
				lineNumber += 1;
				AbstractLine outLine = lineCreator.newLine();
				
				for (fldNum = 0; fldNum < fieldCount; fldNum++) {
					v = "";
					IFieldValue fieldValue = outLine.getFieldValue(0, fldNum);
					
					if (fieldValue.isFieldInRecord()) {
						if (fieldMapping.get(fldNum) == null) {
							fieldValue.set(CommonBits.NULL_VALUE);
						} else {
							IFieldValue sfv = inLine.getFieldValue(fieldMapping.get(fldNum));
							
							v = null;
							if (sfv.isFieldInRecord()) {
								v = sfv.asString();
							}

							if (v == null || v.length() == 0) {
								fieldValue.set(CommonBits.NULL_VALUE);
							} else {
								fieldValue.set(v);
							}
						}
					}
				}
				writer.write(outLine);
				inLine = reader.read();
			}
		} catch (Exception e) {
//			System.out.println("Error at Line" + lineNumber + ", field_Number: " + (fldNum + 1) + " Value=" + v + " " + e);
//			System.out.println();
			System.err.println("Error at Line" + lineNumber + ", field_Number: " + (fldNum + 1) + " Value=" + v + " " + e);
			System.err.println();
			System.err.println();
			e.printStackTrace();
			
			throw new RuntimeException("Error at Line: " + lineNumber, e);
		} finally {
			writer.close();
			reader.close();
		}
		
		return lineNumber;
	}
	
	
	
	private static INewLineCreator newCreator(LayoutDetail schema) {
		return new LineCreator(schema,  LineIOProvider.getInstance().getLineProvider(schema));
	}
	private static class LineCreator implements INewLineCreator {

		private final LayoutDetail schema;
		private final LineProvider provider;
		
		
		protected LineCreator(LayoutDetail schema, LineProvider provider) {
			super();
			this.schema = schema;
			this.provider = provider;
		}


		/* (non-Javadoc)
		 * @see net.sf.JRecord.def.IO.builders.INewLineCreator#newLine()
		 */
		@Override
		public AbstractLine newLine() throws IOException {

			return provider.getLine(schema);
		}
		
	}
}
