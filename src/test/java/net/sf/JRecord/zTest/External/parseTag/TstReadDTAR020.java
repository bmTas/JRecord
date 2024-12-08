package net.sf.JRecord.zTest.External.parseTag;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.zTest.External.CopybookAccess;
import net.sf.JRecord.zTest.External.test.data.FileLine;

public class TstReadDTAR020  {
	
	private static final FileLine[] expectedLines = {
			new FileLine(0, "63604808", "20", "40118", "170", "1", "4.87"),
			new FileLine(0, "69684558", "20", "40118", "280", "1", "19.00"),
			new FileLine(0, "69684558", "20", "40118", "280", "-1", "-19.00"),
			new FileLine(0, "69694158", "20", "40118", "280", "1", "5.01"),
			new FileLine(0, "62684671", "20", "40118", "685", "1", "69.99"),
			new FileLine(0, "62684671", "20", "40118", "685", "-1", "-69.99"),
			new FileLine(0, "61664713", "59", "40118", "335", "1", "17.99"),
			new FileLine(0, "61664713", "59", "40118", "335", "-1", "-17.99"),
			new FileLine(0, "61684613", "59", "40118", "335", "1", "12.99"),
			new FileLine(0, "68634752", "59", "40118", "410", "1", "8.99"),
			new FileLine(0, "60694698", "59", "40118", "620", "1", "3.99"),
			new FileLine(0, "60664659", "59", "40118", "620", "1", "3.99"),
			new FileLine(0, "60614487", "59", "40118", "878", "1", "5.95"),
			new FileLine(0, "68654655", "166", "40118", "60", "1", "5.08"),
			new FileLine(0, "69624033", "166", "40118", "80", "1", "18.19"),
			new FileLine(0, "60604100", "166", "40118", "80", "1", "13.30"),
			new FileLine(0, "68674560", "166", "40118", "170", "1", "5.99"),
	};

	@Test
	public void testReadDTAR020() throws Exception {
		ExternalRecord dtar020Record = CopybookAccess.getDTAR020();
		checkFile(dtar020Record.asLayoutDetail());
	}

	@Test
	public void testReadDTAR020a() throws Exception {
		ExternalRecord dtar020Record = CopybookAccess.getDTAR020a();
		checkFile(dtar020Record.asLayoutDetail());
	}

	private void checkFile(LayoutDetail schema) throws IOException {
		AbstractLineReader reader = LineIOProvider.getInstance().getLineReader(schema);
		
		//FileLine.print(reader);
		AbstractLine line;
		reader.open(CopybookAccess.getDTAR020BinFileName(), schema);
		
		int index = 0;
		while( (line = reader.read()) != null) {
			assertTrue(index < expectedLines.length);
			expectedLines[index++].checkLine(line);
		}
		reader.close();
		assertEquals(expectedLines.length, index);
	}
}
