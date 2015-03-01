package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Numeric.ICopybookDialects;

public class WriteSampleFile {

	public static void main(String[] args)  throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource("OccursDepending1.cbl").getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE);
		AbstractLineWriter w = ioBuilder.newWriter("G:\\Temp\\OccursDependingOn.txt");
		try {
			for (int i = 0; i < 16; i++) {
				for (int j = 0; j < 12; j++) {
					w.write(Code.generateLine(ioBuilder.newLine(), i, j));
				}
			}
		} finally {
			w.close();
		}
	}
}
