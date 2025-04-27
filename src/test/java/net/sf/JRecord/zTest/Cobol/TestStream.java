package net.sf.JRecord.zTest.Cobol;

import static org.junit.jupiter.api.Assertions.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.CommonCodeTestCopybooksAndData;

class TestStream {

	private static final BigDecimal DECIMAL_10 = BigDecimal.valueOf(10);
	static final String[][] EXPECTED_NEGATIVE_QUANTITIES = {
			{"69684558", "20", "280", "-1", "-19.00", },
				{"62684671", "20", "685", "-1", "-69.99",  },
				{"61664713", "59", "335", "-1", "-17.99",  },	
	};
	
	static final String[][] EXPECTED_PRICE_GREATER_THAN_10 = {
			{"69684558", "20", "280", "1", "19.00",  },
			{"62684671", "20", "685", "1", "69.99",  },
			{"61664713", "59", "335", "1", "17.99",  },
			{"61684613", "59", "335", "1", "12.99",  },
			{"69624033", "166", "80", "1", "18.19",  },
			{"60604100", "166", "80", "1", "13.30",  },
	};

	
	@Test
	void testFilter1() throws IOException {
		List<AbstractLine> filteredList = getDTAR020List().filter(
						line -> line.getFieldValue("DTAR020-QTY-SOLD").asInt() < 0
					).toList();
		
		check(EXPECTED_NEGATIVE_QUANTITIES, filteredList);
		//printList(filteredList);
	}

	
	@Test
	void testFilter2() throws IOException {
		List<AbstractLine> filteredList = getDTAR020List().filter(
						line -> line.getFieldValue("DTAR020-SALE-PRICE").asBigDecimal().compareTo(DECIMAL_10) > 0
					).toList();
		
		check(EXPECTED_PRICE_GREATER_THAN_10, filteredList);
		printList(filteredList);
	}


	private Stream<AbstractLine> getDTAR020List() throws IOException, FileNotFoundException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(
				JRecordInterface1.COBOL.newCobolCopybookReader()
					.setCopybookName("DTAR020")
					.addFreeFormatCobolText(CommonCodeTestCopybooksAndData.DTAR020_COPYBOOK))
				.setFont("cp037")
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH );
		
		Stream<AbstractLine> stream = iob.newReader(CommonCodeTestCopybooksAndData.DTAR020_TST1_DATA_FILE)
										 .stream();
		return stream;
	}


	private void check(String[][] expected, List<AbstractLine> filteredList) {
		assertEquals(expected.length, filteredList.size());
		
		for (int i =0; i < expected.length; i++) {
			AbstractLine l = filteredList.get(i);
			String[] expectedRow = expected[i];
			
			assertEquals(expectedRow[0], l.getFieldValue("DTAR020-KEYCODE-NO").asString());
			assertEquals(expectedRow[1], l.getFieldValue("DTAR020-STORE-NO").asString());
			assertEquals(expectedRow[2], l.getFieldValue("DTAR020-DEPT-NO").asString());
			assertEquals(expectedRow[3], l.getFieldValue("DTAR020-QTY-SOLD").asString());
			assertEquals(expectedRow[4], l.getFieldValue("DTAR020-SALE-PRICE").asString());
		}
	}
	
	
	private void printList(List<AbstractLine> list) {
		for (AbstractLine l : list) {
			System.out.println(""
					+ "\t{"
					+ format(l.getFieldValue("DTAR020-KEYCODE-NO"))
					+ format(l.getFieldValue("DTAR020-STORE-NO"))
					+ format(l.getFieldValue("DTAR020-DEPT-NO"))
					+ format(l.getFieldValue("DTAR020-QTY-SOLD"))
					+ format(l.getFieldValue("DTAR020-SALE-PRICE"))
					+ " },"
					);
			
		}
	}


	String format(IFieldValue v) {
		return "\"" + v.asString() + "\", ";
	}
}
