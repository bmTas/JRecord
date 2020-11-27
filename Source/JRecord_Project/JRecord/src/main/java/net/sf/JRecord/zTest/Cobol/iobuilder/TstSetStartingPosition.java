package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;

public class TstSetStartingPosition extends TestCase {

	private static final String VARIABLE_SECTION_CBL = "ZVariableSection.cbl";
	private static final String RECORD_A_CBL = "RecordA.cbl";
	private static String DTAR020_COPYBOOK
			= "              03  DTAR020-KCODE-STORE-KEY.                        \n"
			+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).           \n"
			+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3. \n"
			+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3. \n"
			+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3. \n"
			+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3. \n"
			+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3. \n";

	private static String RECORD_A_COPYBOOK
			= "          01  Detail-Record-A.\n"
            + "              05 Record-Type                 Pic X.\n"
            + "                 88 Detail-Record         value 'A'.\n"
            + "              05 Field-1a                    Pic X(11).\n"
            + "              05 Field-2a                    Pic X(12).\n"
            + "              05 Field-3a                    Pic X(11).\n"
            + "              05 Field-4a                    Pic X(44).\n";
	
	private static String VARIABLE_FIELD = "Variable-Part";
	private static String ZOTHER_COPYBOOK
			= "          01  Detail-Record-A.\n"
		    + "              05 Header-Details              Pic X(110).\n"
		    + "              05 " + VARIABLE_FIELD +"               Pic X(100).\n";
	
	int[] dtar020Positions = { 1, 9, 11, 15, 17, 22 };
	int[] recordAPositions = {1, 2, 13, 25, 36 };

	/**
	 * Test setting initial position using Cobol Copybook being read from a Stream
	 * @throws IOException
	 */
	public void testBuildUsingStream() throws IOException {
		checkStreamIOBuilder(10);
		checkStreamIOBuilder(20);
		checkStreamIOBuilder(45);
	}

	/**
	 * Check setting initial position using Cobol Copybook being read from a Stream
	 * @param pos position of the second copybook
	 * @throws IOException
	 */
	private void checkStreamIOBuilder(int pos) throws IOException {
		ICobolMultiCopybookIOBuilder iob = JRecordInterface1.COBOL
				.newMultiCopybookIOBuilder("MultiCopybook")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.addCopyBook(new ByteArrayInputStream(DTAR020_COPYBOOK.getBytes()), "DTAR020")
					.addCopyBook(new ByteArrayInputStream(RECORD_A_COPYBOOK.getBytes()),  "DETAIL")
						.setStartingPosition(pos);

		checkXRec(iob.getExternalRecord(), pos);
	}
	
	/**
	 * Test setting initial position using Cobol Copybook being read using a reader
	 * @throws IOException
	 */
	public void testBuildUsingReader() throws IOException {
		checkReaderIOBuilder(10);
		checkReaderIOBuilder(20);
		checkReaderIOBuilder(45);
	}

	/**
	 * Check setting initial position using Cobol Copybook being read using a reader
	 * @param pos initial position
	 * @throws IOException
	 */
	private void checkReaderIOBuilder(int pos) throws IOException {
		ICobolMultiCopybookIOBuilder iob = JRecordInterface1.COBOL
				.newMultiCopybookIOBuilder("MultiCopybook")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.addCopyBook(new StringReader(DTAR020_COPYBOOK), "DTAR020")
					.addCopyBook(new StringReader(RECORD_A_COPYBOOK),  "DETAIL")
						.setStartingPosition(pos);

		checkXRec(iob.getExternalRecord(), pos);
	}

	/**
	 * Test setting initial position using Cobol Copybook being read using a reader
	 * @throws IOException
	 */
	public void testBuildUsingFile() throws IOException {
		checkFileIOBuilder(10);
	}

	/**
	 * Check setting initial position using Cobol Copybook being read using a reader
	 * @param pos initial position
	 * @throws IOException
	 */
	private void checkFileIOBuilder(int pos) throws IOException {
		String filename1 = TstSetStartingPosition.class.getResource(RECORD_A_CBL).getFile();
		ICobolMultiCopybookIOBuilder iob = JRecordInterface1.COBOL
				.newMultiCopybookIOBuilder("MultiCopybook")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.addCopyBook(new StringReader(DTAR020_COPYBOOK), "DTAR020")
					.addCopyBook(filename1)
						.setStartingPosition(pos);

		checkXRec(iob.getExternalRecord(), pos);
	}

	private void checkXRec(ExternalRecord xr, int pos) {
		int p = pos-1;
		
		ExternalRecord record = xr.getRecord(0);
		for (int i = 0; i < dtar020Positions.length; i++) {
			assertEquals(pos + ": " + i, dtar020Positions[i], record.getRecordField(i).getPos());
		}
		
		record = xr.getRecord(1);
		for (int i = 0; i < recordAPositions.length; i++) {
			assertEquals(pos + ": " + i, recordAPositions[i] + p, record.getRecordField(i).getPos());
		}
	}
	
	/**
	 * Testing setting starting position using a field (Stream)
	 * @throws IOException
	 */
	public void testFieldBuildUsingStream() throws IOException {
		String zOther = "ZOther";
		ICobolMultiCopybookIOBuilder iob = JRecordInterface1.COBOL
				.newMultiCopybookIOBuilder("MultiCopybook")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.addCopyBook(new ByteArrayInputStream(ZOTHER_COPYBOOK.getBytes()), zOther)
					.addCopyBook(new ByteArrayInputStream(RECORD_A_COPYBOOK.getBytes()),  "DETAIL")
						.setStartingPositionToField(zOther, VARIABLE_FIELD)
					.addCopyBook(new ByteArrayInputStream(DTAR020_COPYBOOK.getBytes()),  "DTAR020")
						.setStartingPositionToField(zOther, VARIABLE_FIELD);

		ExternalRecord xr = iob.getExternalRecord();
		checkXRec2(xr);
		checkXRec2(xr, 2, dtar020Positions);
	}		
	
	/**
	 * Testing setting starting position using a field (Reader)
	 * @throws IOException
	 */
	public void testFieldBuildUsingReader() throws IOException {
		String zOther = "ZOther";
		ICobolMultiCopybookIOBuilder iob = JRecordInterface1.COBOL
				.newMultiCopybookIOBuilder("MultiCopybook")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.addCopyBook(new StringReader(ZOTHER_COPYBOOK), zOther)
					.addCopyBook(new StringReader(RECORD_A_COPYBOOK),  "DETAIL")
						.setStartingPositionToField(zOther, VARIABLE_FIELD)
					.addCopyBook(new StringReader(DTAR020_COPYBOOK),  "DTAR020")
						.setStartingPositionToField(zOther, VARIABLE_FIELD);

		ExternalRecord xr = iob.getExternalRecord();
		checkXRec2(xr);
		checkXRec2(xr, 2, dtar020Positions);
	}		
	
	/**
	 * Testing setting starting position using a field (file)
	 * @throws IOException
	 */
	public void testFieldBuildUsingFile() throws IOException {
		String filename1 = TstSetStartingPosition.class.getResource(VARIABLE_SECTION_CBL).getFile();
		String filename2 = TstSetStartingPosition.class.getResource(RECORD_A_CBL).getFile();
		ICobolMultiCopybookIOBuilder iob = JRecordInterface1.COBOL
				.newMultiCopybookIOBuilder("MultiCopybook")
					.setDialect(ICopybookDialects.FMT_MAINFRAME)
					.addCopyBook(filename1)
					.addCopyBook(filename2)
						.setStartingPositionToField(filename1, VARIABLE_FIELD)
					.addCopyBook(new ByteArrayInputStream(DTAR020_COPYBOOK.getBytes()),  "DTAR020")
						.setStartingPositionToField(filename1, VARIABLE_FIELD)
					.addCopyBook(new StringReader(DTAR020_COPYBOOK),  "DTAR020")
						.setStartingPositionToField(filename1, VARIABLE_FIELD);

		ExternalRecord xr = iob.getExternalRecord();
		checkXRec2(xr);
		checkXRec2(xr, 2, dtar020Positions);
		checkXRec2(xr, 3, dtar020Positions);
	}		

	private void checkXRec2(ExternalRecord xr) {
		checkXRec2(xr, 1, recordAPositions);
	}		

	private void checkXRec2(ExternalRecord xr, int idx, int [] fieldPositions) {
		int p = 110;
		
		ExternalRecord record = xr.getRecord(idx);
		for (int i = 0; i < fieldPositions.length; i++) {
			assertEquals(": " + i, fieldPositions[i] + p, record.getRecordField(i).getPos());
		}
	}


}
