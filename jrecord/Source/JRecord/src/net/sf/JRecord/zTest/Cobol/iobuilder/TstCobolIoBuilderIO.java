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

package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;


/**
 * Checking reading and writing files with IOBuilders
 * 
 * @author Bruce Martin
 *
 */
public class TstCobolIoBuilderIO extends TestCase {

	private static final int DTAR020_RECORD_LENGTH = 27;

	private static final String[][] EXPECTED = {
		{"63604808", "20", "40118", "170", "1", "4.87"},
		{"69684558", "20", "40118", "280", "1", "19.00"},
		{"69684558", "20", "40118", "280", "-1", "-19.00"},
		{"69694158", "20", "40118", "280", "1", "5.01"},
		{"62684671", "20", "40118", "685", "1", "69.99"},
		{"62684671", "20", "40118", "685", "-1", "-69.99"},
		{"61664713", "59", "40118", "335", "1", "17.99"},
		{"61664713", "59", "40118", "335", "-1", "-17.99"},
		{"61684613", "59", "40118", "335", "1", "12.99"},
		{"68634752", "59", "40118", "410", "1", "8.99"},
		{"60694698", "59", "40118", "620", "1", "3.99"},
		{"60664659", "59", "40118", "620", "1", "3.99"},
		{"60614487", "59", "40118", "878", "1", "5.95"},
		{"68654655", "166", "40118", "60", "1", "5.08"},
		{"69624033", "166", "40118", "80", "1", "18.19"},
		{"60604100", "166", "40118", "80", "1", "13.30"},
		{"68674560", "166", "40118", "170", "1", "5.99"},
	};
	
	private static byte[] COPBOOK_BYTES
			=("              03  DTAR020-KCODE-STORE-KEY.                        \n"
			+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).           \n"
			+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3. \n"
			+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3. \n"
			+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3. \n"
			+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3. \n"
			+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3. \n")
			
					.getBytes();

	
	@SuppressWarnings("deprecation")
	private static final FieldDetail[] EXPECTED_DTAR020 = {
		bldType("DTAR020-KEYCODE-NO", 1, 8, 0, 0, "CP037"),
		bldType("DTAR020-STORE-NO", 9, 2, 0, Type.ftPackedDecimalSmall, "CP037"),
		bldType("DTAR020-DATE", 11, 4, 0, Type.ftPackedDecimalSmall, "CP037"),
		bldType("DTAR020-DEPT-NO", 15, 2, 0, Type.ftPackedDecimalSmall, "CP037"),
		bldType("DTAR020-QTY-SOLD", 17, 5, 0, Type.ftPackedDecimalSmall, "CP037"),
		bldType("DTAR020-SALE-PRICE", 22, 6, 2, Type.ftPackedDecimalSmall, "CP037"),
	};
	
	public void testDTAR020SchemaLoad1a() throws RecordException, IOException {
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
					.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
			
		tstDTAR020SchemaLoad1(ioBuilder);
	}

	public void testDTAR020SchemaLoad1b() throws RecordException, IOException {
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
					.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
			
		tstDTAR020SchemaLoad1(ioBuilder);
	}

	/**
	 * @param ioBuilder
	 * @throws RecordException
	 * @throws IOException
	 */
	private void tstDTAR020SchemaLoad1(ICobolIOBuilder ioBuilder)
			throws RecordException, IOException {
		ioBuilder	.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037");
		LayoutDetail l = ioBuilder.getLayout();
		RecordDetail r = l.getRecord(0);
		
		for (int i = 0; i < r.getFieldCount(); i++) {
			FieldDetail field = r.getField(i);
			FieldDetail ef = EXPECTED_DTAR020[i];
			
			assertEquals(ef.getName(), field.getName());
			assertEquals(ef.getPos(),  field.getPos());
			assertEquals(ef.getLen(),  field.getLen());
			assertEquals(ef.getDecimal(), field.getDecimal());
			assertEquals(ef.getType(), field.getType());
//				System.out.println("\tbldType(\"" + field.getName() 
//						+ "\", " + field.getPos()
//						+ ", "   + field.getLen()
//						+ ", "   + field.getDecimal()
//						+ ", "   + field.getType()
//						+ ", \"" + l.getFontName() 
//						+ "\"),"
//				);
		}
		
		chkDTAR020_Layout(l);
	}
	   
	
	public void testDTAR020SchemaLoad2a() throws RecordException, IOException {
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
		tstDTAR020SchemaLoad2(ioBuilder);
   }

	
	public void testDTAR020SchemaLoad2b() throws RecordException, IOException {
		ICobolIOBuilder ioBuilder =JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
		tstDTAR020SchemaLoad2(ioBuilder);
   }

	/**
	 * @param ioBuilder
	 * @throws RecordException
	 * @throws IOException
	 */
	private void tstDTAR020SchemaLoad2(ICobolIOBuilder ioBuilder)
			throws RecordException, IOException {
		ioBuilder	.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037")
					.setDropCopybookNameFromFields(true);
    	LayoutDetail l = ioBuilder.getLayout();
    	RecordDetail r = l.getRecord(0);
    	
    	for (int i = 0; i < r.getFieldCount(); i++) {
    		FieldDetail field = r.getField(i);
    		FieldDetail ef = EXPECTED_DTAR020[i];
    		
    		assertEquals(ef.getName().substring(8), field.getName());
       		assertEquals(ef.getPos(),  field.getPos());
       		assertEquals(ef.getLen(),  field.getLen());
       		assertEquals(ef.getDecimal(), field.getDecimal());
       		assertEquals(ef.getType(), field.getType());
    	}
    	
    	chkDTAR020_Layout(l);
	}

    private void chkDTAR020_Layout(LayoutDetail l) { 	
	    	assertEquals(EXPECTED_DTAR020.length, l.getRecord(0).getFieldCount());
	    	assertEquals("CP037", l.getFontName());
	    	assertEquals(1, l.getRecordCount());
	    	assertEquals(DTAR020_RECORD_LENGTH, l.getMaximumRecordLength());
	    	assertEquals(Constants.IO_FIXED_LENGTH, l.getFileStructure());
	    	assertEquals(Constants.rtBinaryRecord, l.getLayoutType());
	 }

	/**
	 * Check Reading a File / Stream with an IOBuilder
	 */
	public void testReader1() throws FileNotFoundException, IOException, RecordException {
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
										.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
											.setFileOrganization(Constants.IO_FIXED_LENGTH)
											.setFont("CP037");
		String dataFile = this.getClass().getResource("DTAR020_tst1.bin").getFile();
		tstReader(ioBuilder.newReader(dataFile));
		tstReader(ioBuilder.newReader(new FileInputStream(dataFile)));
	}
	
	
	/**
	 * Check Reading a File / Stream with an IOBuilder
	 */
	public void testReader2() throws FileNotFoundException, IOException, RecordException {
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
										.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
											.setFileOrganization(Constants.IO_FIXED_LENGTH)
											.setFont("CP037");
		String dataFile = this.getClass().getResource("DTAR020_tst1.bin").getFile();
		tstReader(ioBuilder.newReader(dataFile));
		tstReader(ioBuilder.newReader(new FileInputStream(dataFile)));
	}

	public void testReader3() throws FileNotFoundException, IOException, RecordException {
		tstFixedLengthReader(DTAR020_RECORD_LENGTH);
		tstFixedLengthReader(31);
		tstFixedLengthReader(37);
		tstFixedLengthReader(-11);
	}

	/**
	 * @param recordLength
	 * @throws IOException
	 * @throws RecordException
	 */
	public void tstFixedLengthReader(int recordLength) throws IOException,
			RecordException {
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setRecordLength(recordLength)
					.setFont("CP037");
		//String dataFile = this.getClass().getResource("DTAR020_tst1.bin").getFile();
		
		int reclen = Math.max(DTAR020_RECORD_LENGTH, recordLength);
		assertEquals(recordLength, ioBuilder.getExternalRecord().getRecordLength());
		assertEquals(reclen, ioBuilder.getLayout().getMaximumRecordLength());

		
		List<AbstractLine> lines = getLines(ioBuilder);
		byte[] bytes = writeList(ioBuilder, lines);

		tstReader(ioBuilder.newReader(new ByteArrayInputStream(bytes)));
	}

	/**
	 * Check writing a Fixed Width Records to  Filename and a Stream
	 */
	public void testFixedWriter1() throws IOException, RecordException {
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037");
		tstWrite(ioBuilder, DTAR020_RECORD_LENGTH);
	}

	/**
	 * Check writing a Fixed Width Records to  Filename and a Stream
	 */
	public void testFixedWriter2() throws IOException, RecordException {
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037");
		tstWrite(ioBuilder, DTAR020_RECORD_LENGTH);
	}

	public void testFixedWriter3() throws IOException, RecordException {
		tstFixedWidthWriter(DTAR020_RECORD_LENGTH);
		tstFixedWidthWriter(37);
		tstFixedWidthWriter(31);
		tstFixedWidthWriter(-121);
	}

	/**
	 * @param recordLength
	 * @throws IOException
	 * @throws RecordException
	 */
	public void tstFixedWidthWriter(int recordLength) throws IOException,
			RecordException {
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setRecordLength(recordLength)
					.setFont("CP037");
		
		int reclen = Math.max(DTAR020_RECORD_LENGTH, recordLength);
		assertEquals(recordLength, ioBuilder.getExternalRecord().getRecordLength());
		assertEquals(reclen, ioBuilder.getLayout().getMaximumRecordLength());
	
		
		tstWrite(ioBuilder, DTAR020_RECORD_LENGTH);
	}


	private void tstWrite(ICobolIOBuilder ioBuilder, int tstLength) throws IOException, RecordException {
		List<AbstractLine> lines = getLines(ioBuilder);
		byte[] bytes = writeList(ioBuilder, lines);
		int recordLength = ioBuilder.getLayout().getMaximumRecordLength();
		byte[] rec = new byte[recordLength];
		
		for (int i = 0; i < lines.size(); i++) {
			System.arraycopy(bytes, i* recordLength, rec, 0, recordLength);
			boolean ok = true;
			byte[] b = lines.get(i).getData();
			
			for (int j = 0; j < tstLength; j++) {
				if (b[j] != rec[j]) {
					ok = false;
					System.out.println("  ** Char: " + j + " " + b[j] + " <> " + rec[j]);
				}
			}
			assertTrue("Testing Line: " + i, ok);
		}
		
		tstReader(ioBuilder.newReader(new ByteArrayInputStream(bytes)));
		
		String tempFile = TstConstants.TEMP_DIRECTORY + "fbDTAR020_tst1.bin";
		writeLines(lines, ioBuilder.newWriter(tempFile));
		tstReader(ioBuilder.newReader(tempFile));
	}
	
	/**
	 * Check writing Mainframe VB Records to both a Filename and a Stream
	 */
	public void testVBWriter1() throws IOException, RecordException {
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
		tstVBWriter(ioBuilder);
	}

	/**
	 * Check writing Mainframe VB Records to both a Filename and a Stream
	 */
	public void testVBWriter2() throws IOException, RecordException {
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
		tstVBWriter(ioBuilder);
	}
	/**
	 * @param ioBuilder
	 * @throws IOException
	 * @throws RecordException
	 * @throws FileNotFoundException
	 */
	private void tstVBWriter(ICobolIOBuilder ioBuilder) throws IOException,
			RecordException, FileNotFoundException {
		ioBuilder	.setFileOrganization(Constants.IO_VB)
					.setFont("CP037");
		
		List<AbstractLine> lines = getLines(ioBuilder);
		byte[] bytes = writeList(ioBuilder, lines);
		int recordLength = ioBuilder.getLayout().getMaximumRecordLength();
		byte[] rec = new byte[recordLength];

		tstReader(ioBuilder.newReader(new ByteArrayInputStream(bytes)));

		for (int i = 0; i < lines.size(); i++) {
			int recordStart = i * (recordLength + 4);
			System.arraycopy(bytes, recordStart + 4, rec, 0, recordLength);
			assertTrue("Testing Line: " + i, Arrays.equals(lines.get(i).getData(), rec));
			assertEquals(0, bytes[recordStart]);
			assertEquals(recordLength + 4, bytes[recordStart+1]);
			assertEquals(0, bytes[recordStart+2]);
			assertEquals(0, bytes[recordStart+3]);
		}
		
		String tempFile = TstConstants.TEMP_DIRECTORY + "vbDTAR020_tst1.bin";
		writeLines(lines, ioBuilder.newWriter(tempFile));
		tstReader(ioBuilder.newReader(tempFile));
		tstReader(ioBuilder.newReader(new FileInputStream(tempFile)));
	}
	
	
	/**
	 * Check writing Gnu-Cobol VB Records to a Stream
	 */
	public void testGnuCobolVBWriter1() throws IOException, RecordException {
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
		tstGnuCobolVBWriter(ioBuilder);
	}
	
	public void testGnuCobolVBWriter2() throws IOException, RecordException {
		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020");
		tstGnuCobolVBWriter(ioBuilder);
	}


	/**
	 * @param ioBuilder
	 * @throws IOException
	 * @throws RecordException
	 */
	private void tstGnuCobolVBWriter(ICobolIOBuilder ioBuilder)
			throws IOException, RecordException {
		ioBuilder	.setFileOrganization(Constants.IO_VB_GNU_COBOL)
					.setFont("CP037");
		
		List<AbstractLine> lines = getLines(ioBuilder);
		byte[] bytes = writeList(ioBuilder, lines);
		int recordLength = ioBuilder.getLayout().getMaximumRecordLength();
		byte[] rec = new byte[recordLength];

		tstReader(ioBuilder.newReader(new ByteArrayInputStream(bytes)));

		for (int i = 0; i < lines.size(); i++) {
			int recordStart = i * (recordLength + 4);
			System.arraycopy(bytes, recordStart + 4, rec, 0, recordLength);
			assertTrue("Testing Line: " + i, Arrays.equals(lines.get(i).getData(), rec));
			assertEquals(0, bytes[recordStart]);
			assertEquals(recordLength, bytes[recordStart+1]);
			assertEquals(0, bytes[recordStart+2]);
			assertEquals(0, bytes[recordStart+3]);
		}
	}


	private byte[] writeList(ICobolIOBuilder ioBuilder, List<AbstractLine> lines) throws IOException, RecordException {
		ByteArrayOutputStream os = new ByteArrayOutputStream((lines.size() + 5) * 20 );
		AbstractLineWriter writer = ioBuilder.newWriter(os);
		writeLines(lines, writer);
		
		return os.toByteArray();
	}

	/**
	 * @param lines
	 * @param writer
	 * @throws IOException
	 */
	private void writeLines(List<AbstractLine> lines, AbstractLineWriter writer)
			throws IOException {
		for (AbstractLine line : lines) {
			writer.write(line);
		}
		writer.close();
	}
	
	
	@SuppressWarnings("deprecation")
	private List<AbstractLine> getLines(ICobolIOBuilder ioBuilder) throws IOException, RecordException {
		ArrayList<AbstractLine> lines = new ArrayList<AbstractLine>(EXPECTED.length);
		
		for (int i = 0; i < EXPECTED.length; i++) {
			String[] flds = EXPECTED[i];
			AbstractLine line = ioBuilder.newLine();
			for (int j = 0; j < flds.length; j++) {
				line.setField(0, j, flds[j]);
			}
			lines.add(line);
		} 	
		return lines;
	}
	
	private void tstReader(AbstractLineReader r) throws IOException {
		LayoutDetail schema = r.getLayout();
		RecordDetail rec = schema.getRecord(0);
		AbstractLine line;
		int lineNo = 0;
		
		while ((line = r.read()) != null) {
			for (int i = 0; i <rec.getFieldCount(); i++) {
				assertEquals("Line=" + lineNo + ", Field=" + rec.getField(i).getName(), EXPECTED[lineNo][i], line.getFieldValue(0, i).asString());
			}
			lineNo+=1;
		}
		r.close();
	}
	
	private static FieldDetail bldType(String name, int pos, int len, int decimal, int type, String font) {
		FieldDetail fd = new FieldDetail(name, "", type, decimal, font, 0, "");
		fd.setPosLen(pos, len);
		
		return fd;
	}
}
