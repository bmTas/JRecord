package net.sf.JRecord.zTest.io.continuous;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class TstDataContinuousMultiByteFonts {

	private static final String LINE_1 = "010001 0120250916";
	private static final String LINE_2 = "010001 052025091620250917202509182025091920250920";
	private static final String LINE_3 = "010001 1020250916202509172025091820250919202509202025101620251017202510182025101920251020";
	private static final String LINE_4 = "010001 112025091620250917202509182025091920250920202510162025101720251018202510192025102020251212";

	private static final String LINE_1_A = "¢10001 0120250916";
	private static final String LINE_2_A = "£10001 052025091620250917202509182025091920250920";

	private static final String COPY_BOOK = ""
			+ "              10  P01-AREA.                                          \n"
			+ "                  15  P01-ID.                                        \n"
			+ "                      20  P01-SEG-ID             PIC  X(03).         \n"
			+ "                      20  P01-GROUP-ID           PIC  X(03).         \n"
			+ "                  15  FILLER                     PIC  X(01).         \n"
			+ "                  15  P01-COUNT                  PIC  9(02).         \n"
			+ "                  15  P01-DISCOUNTABLE-PERIOD OCCURS 1 TO 10 TIMES   \n"
			+ "                                              DEPENDING ON P01-COUNT.\n"
			+ "                      20  P01-IDO-P-K-HAZIME.                        \n"
			+ "                          25  P01-K-H-YY.                            \n"
			+ "                              30  P01-K-H-YY-R                       \n"
			+ "                                                 PIC  9(04).         \n"
			+ "                          25  P01-K-H-MM.                            \n"
			+ "                              30  P01-K-H-MM-R                       \n"
			+ "                                                 PIC  9(02).         \n"
			+ "                          25  P01-K-H-DD.                            \n"
			+ "                              30  P01-K-H-DD-R                       \n"
			+ "                                                 PIC  9(02).         \n"
			+ "                                                                     ";
	
	private static final String DATA = ""
			+ LINE_1
			+ LINE_2
			+ LINE_3
			+ LINE_4;
	
	private static final String DATA_A = ""
			+ LINE_1_A
			+ LINE_2_A;

	private final String encoding;
//
	public TstDataContinuousMultiByteFonts(String encoding) {
		super();
		this.encoding = encoding;
	}
	
	public InputStream getDataStream() throws UnsupportedEncodingException {
		return new ByteArrayInputStream(DATA.getBytes(encoding));
	}
	public InputStream getAlternateDataStream() throws UnsupportedEncodingException {
		return new ByteArrayInputStream(DATA_A.getBytes(encoding));
	}
	
	public ICobolIOBuilder getIoBuilder() {
		return JRecordInterface1.COBOL
                .newIOBuilder(new StringReader(COPY_BOOK), "TestCopybook")
                   .setFont(encoding) 
                   .setFileOrganization(IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER)
                   //.setSplitCopybook(CopybookLoader.SPLIT_NONE)
                       ;  
	}
	
	
	public List<String> getExpectedLines() {
		return Arrays.asList(LINE_1, LINE_2, LINE_3, LINE_4);
	}
	
	public List<String> getAlternateExpectedLines() {
		return Arrays.asList(LINE_1_A, LINE_2_A);
	}
}
