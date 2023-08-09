package net.sf.JRecord.zTest.cobol2csv.sr;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.cbl2csv.Cobol2CsvInterface;
import net.sf.JRecord.cbl2csv.imp.sr.FieldConversion;
import net.sf.JRecord.cbl2csv.imp.sr.IRetrieveFieldValue;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zData.Data;

public class CreateTestData {

	private static final byte[][] VALID_DTAR020_LINES = new byte[12][];
//	private static final byte[] VALID_DTAR020_DATA;
	
	private static final ICobolIOBuilder iob;

	static {
		iob = JRecordInterface1.COBOL.newIOBuilder(Data.DTAR020_COPYBOOK_FILE_NAME)
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
				.setFont("cp037");
		try {
			AbstractLineReader reader = iob.newReader("/home/bruce/.RecordEditor/HSQLDB/SampleFiles/DTAR020.bin");
			int i = 0;
			AbstractLine line;
			while ( i < VALID_DTAR020_LINES.length && (line = reader.read()) != null) {
				VALID_DTAR020_LINES[i++] = line.getData();
			}
			reader.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		
//		VALID_DTAR020_DATA = toBytes(VALID_DTAR020_LINES);
	}
	
	private static byte[] toBytes(byte[][] lines) {
		int recordLength = lines[0].length;
		byte[] ret = new byte[lines.length * recordLength];
		for (int i = 0; i < lines.length; i++) {
			System.arraycopy(lines[i], 0, ret, i * recordLength, recordLength);
		}
		
		return ret;
	}
	
	public static byte[] validDtar020Data() {
		return toBytes(VALID_DTAR020_LINES);
	}
	
	public static byte[] invalidDtar020Data() throws IOException {
		byte[][] tmp = new byte[VALID_DTAR020_LINES.length][];
		
		LayoutDetail layout = iob.getLayout();
		
		for (int i = 0; i < VALID_DTAR020_LINES.length; i++) {
			tmp[i] = VALID_DTAR020_LINES[i];
		}
		tmp[3] = VALID_DTAR020_LINES[3].clone();
		
		for (int i = 0; i < tmp[3].length; i++) {
			tmp[3][i] = 0;
		}
		
		for (int i = 1; i < layout.getRecord(0).getFieldCount(); i++) {
			FieldDetail field = layout.getField(0, i);
			tmp[4+i] = VALID_DTAR020_LINES[4+i].clone();
			byte b = (byte) (i < 2 ? 40 : 0);
			for (int j = field.getPos()-1; j < field.getEnd(); j++) {
				tmp[4+i][j] = b;
			}
		}
		
		return toBytes(tmp);
	}
	
	
	public static class RunCbl2CsvSrTest {
		private final byte[] cobolData;
		private String result="";
		
		boolean columnHeadings=true;
		String csvQuote="\"", csvFieldSeperator=",";
		IRetrieveFieldValue fieldConversion = FieldConversion.FIELD_CONVERSION_REPLACE_ERRORS_WITH_NULL;
		
		
		public RunCbl2CsvSrTest(byte[] cobolData) {
			super();
			this.cobolData = cobolData;
//			
//			Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(iob)
//					.addAllFields()
//					.
		}
		
		public void run() throws IOException {
			StringWriter writer = new StringWriter(600);
			
			try {
				Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(iob)
						.setRetrieveField(fieldConversion)
						.addAllFields()
						.setFieldSeperator(csvFieldSeperator)
						.setQuote(csvQuote)
						.setGenerateHeader(columnHeadings)
						.setInputStream(new ByteArrayInputStream(cobolData))
					.writeCsv(writer);
			} finally {
				result = writer.toString();
			}	
		}
		
		public void runDefault() throws IOException {
			StringWriter writer = new StringWriter(600);
			
			try {
				Cobol2CsvInterface.SINGLE_RECORD_FILES.newCobol2CsvSingleRecordBuilder(iob)
						.addAllFields()
						.setInputStream(new ByteArrayInputStream(cobolData))
					.writeCsv(writer);
			} finally {
				result = writer.toString();
			}	
		}

		/**
		 * @return the result
		 */
		public String getResult() {
			return result;
		}
	}
}
