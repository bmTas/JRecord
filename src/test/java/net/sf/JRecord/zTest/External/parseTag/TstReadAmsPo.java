package net.sf.JRecord.zTest.External.parseTag;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.zTest.External.CopybookAccess;
import net.sf.JRecord.zTest.External.test.data.FileLine;

public class TstReadAmsPo {

	private static final FileLine[] expectedLines = {
			new FileLine(0, "H1", "45.349", "6060", "286225", "040909", "00", " ", "200", "050102", "050107", " ", " ", "LADIES KNI", "C", "FT"),
			new FileLine(1, "D1", "7.0000", "0.0002", "2222500000000", "43314531", "2075359", "45614531", "DONKEY 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5043", "1", "5045", "1", "5065", "1", "5076", "1", "5079", "1", "5151", "1", "5072", "1", "", "0", "", "0", "", "0"),
			new FileLine(1, "D1", "4.0000", "148.3200", "0", "5614944", "2075360", "5614944", "MILK 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5045", "1", "5076", "1", "5079", "1", "3331", "49440001", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(1, "D1", "48.0000", "148.3200", "0", "55615071", "2075361", "55615071", "M.ROSE 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5036", "3", "5043", "5", "5045", "5", "3331", "50710003", "5065", "4", "5069", "4", "5076", "4", "5079", "2", "5094", "4", "5128", "3"),
			new FileLine(2, "S1", "5151", "4", "5180", "3", "5072", "2", "5173", "2", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(1, "D1", "4.0000", "148.3200", "0", "55615156", "2075362", "55615156", "AQUA 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5043", "1", "5045", "1", "5065", "1", "3331", "51560001", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(0, "H1", "45.350", "6228", "222227", "040909", "00", " ", "200", "050102", "050107", " ", " ", "LADIES KNI", "C", "FT"),
			new FileLine(1, "D1", "16.0000", "6228148.3200", "2222", "2224531", "2075348", "5614531", "DONKEY 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5019", "1", "5037", "1", "5074", "1", "5078", "1", "5085", "1", "5091", "1", "5093", "1", "5095", "1", "51 D", "ONKEY 24", "-6", "607 SHWL"),
			new FileLine(2, "S1", "5171", "1", "5177", "1", "5089", "1", "5136", "1", "5145", "1", "5096", "1", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(1, "D1", "8.0000", "148.3200", "0", "62224944", "2075349", "65614944", "MILK 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5019", "1", "5037", "1", "5078", "1", "5091", "1", "5093", "1", "5129", "1", "5177", "1", "5145", "1", "", "0", "", "0"),
			new FileLine(1, "D1", "108.0000", "148.3200", "0", "62225071", "2075350", "65615071", "M.ROSE 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5015", "2", "5019", "5", "5033", "2", "5035", "2", "5037", "4", "5052", "2", "5055", "2", "5060", "2", "5070", "2", "5074", "4"),
			new FileLine(2, "S1", "5078", "5", "5081", "2", "5085", "3", "5090", "2", "5091", "4", "5093", "4", "5095", "4", "5129", "4", "5144", "4", "5165", "2"),
			new FileLine(2, "S1", "5303", "2", "5169", "2", "5170", "2", "5171", "3", "5177", "4", "5016", "2", "5089", "4", "5136", "3", "5011", "2", "5046", "2"),
			new FileLine(2, "S1", "5145", "4", "5096", "3", "5154", "2", "5162", "2", "5163", "2", "5164", "2", "5192", "2", "5150", "2", "5175", "2", "", "0"),
			new FileLine(1, "D1", "8.0000", "148.3200", "0", "52225156", "2075351", "55615156", "M.ROSE 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5019", "1", "5037", "1", "5078", "1", "5091", "1", "5093", "1", "5129", "1", "5177", "1", "5145", "1", "", "0", "", "0"),
			new FileLine(0, "H1", "45.351", "6228", "222243", "040909", "00", " ", "200", "050102", "050107", " ", " ", "LADIES KNI", "C", "FT"),
			new FileLine(1, "D1", "6.0000", "148.3200", "0", "45614531", "2075352", "45614531", "DONKEY 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5009", "1", "5021", "1", "5024", "1", "5025", "1", "5026", "1", "5127", "1", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(1, "D1", "3.0000", "148.3200", "0", "45614944", "2075353", "45614944", "MILK 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5009", "1", "5021", "1", "5127", "1", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(1, "D1", "44.0000", "148.3200", "0", "45615071", "2075354", "45615071", "M.ROSE 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5009", "5", "5021", "5", "5024", "4", "5025", "4", "5026", "4", "5047", "3", "5077", "2", "5127", "5", "5134", "4", "5142", "2"),
			new FileLine(2, "S1", "5044", "2", "5071", "2", "5159", "2", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(1, "D1", "3.0000", "148.3200", "0", "45615156", "2075355", "35615156", "AQUA 24-006607 SHWL WRAP CARD"),
			new FileLine(2, "S1", "5009", "1", "5021", "1", "5127", "1", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0"),
			new FileLine(0, "H1", "45.352", "5341", "294915", "041013", "00", " ", "475", "041231", "050107", "P", " ", "WOMENS SHO", "C", "FT"),
			new FileLine(1, "D1", "30.0000", "292.6800", "0", "45846680", "2120736", "45615156", "CONCERTO BLACK LEATHER ANKLE BOOT"),
			new FileLine(2, "S1", "5036", "2", "5043", "3", "5045", "3", "5057", "1", "5065", "2", "5069", "2", "5076", "3", "5079", "1", "5094", "3", "5128", "3"),
			new FileLine(2, "S1", "5151", "3", "5180", "2", "5072", "1", "5173", "1", "", "0", "", "0", "", "0", "", "0", "", "0", "", "0"),
	};
	
	
	
	@Test
	public void testReadAmsPo() throws Exception {
		ExternalRecord amsPo = CopybookAccess.getAmsPo();
		checkFile(amsPo.asLayoutDetail());
	}
	
	@Test
	public void testReadAmsPoMulti() throws Exception {
		ExternalRecord amsPo;
		for (int i = 2; i < 7; i++) {
			if (i != 4) {
				amsPo = CopybookAccess.getAmsPo(i);
				checkFile(amsPo.asLayoutDetail());
			}
		}
	}
	
	@Test
	public void testReadAmsPoXmlSchema() throws Exception {
		ExternalRecord amsPo;
		amsPo = CopybookAccess.getXmlAmsPo();
		checkFile(amsPo.asLayoutDetail());
		
		amsPo = CopybookAccess.getXmlAmsPo2();
		checkFile(amsPo.asLayoutDetail());
	}


	private void checkFile(LayoutDetail schema) throws IOException {
		AbstractLineReader reader = LineIOProvider.getInstance().getLineReader(schema);
		
		//FileLine.print(reader);
		AbstractLine line;
		reader.open(CopybookAccess.getAmsTextFileName(), schema);
		//FileLine.print(reader);
		
		int index = 0;
		while( (line = reader.read()) != null) {
			assertTrue(index < expectedLines.length);
			expectedLines[index++].checkLine(line);
		}
		reader.close();
		assertEquals(expectedLines.length, index);
	}

}
