package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

public class WriteSampleFile {

	public static final int PURCH_COUNT = 16;
	public static final int SALES_COUNT = 5;
	
	
	public static void main(String[] args)  throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource("OccursDepending1.cbl").getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		AbstractLineWriter w = ioBuilder.newWriter(TstConstants.TEMP_DIRECTORY + "OccursDependingOn.txt");
		try {
			for (int purchNum = 0; purchNum < PURCH_COUNT; purchNum++) {
				for (int salesNum = 0; salesNum < SALES_COUNT; salesNum++) {
					w.write(Code.generateLine(ioBuilder.newLine(), purchNum, salesNum));
				}
			}
		} finally {
			w.close();
		}
	}
}
