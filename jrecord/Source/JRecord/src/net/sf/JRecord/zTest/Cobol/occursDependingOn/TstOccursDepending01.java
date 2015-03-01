package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.IOException;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Numeric.ICopybookDialects;
import junit.framework.TestCase;

public class TstOccursDepending01 extends TestCase {

	private static final String MONTHS = "months";
	private static final String WEEK_NO = "week-no";

	public void testPositionCalc1() throws IOException, RecordException {
		tstPosition("OccursDepending1.cbl", Constants.IO_STANDARD_TEXT_FILE);
	}
	
	public void testPositionCalc2() throws IOException, RecordException {
		tstPosition("OccursDepending2.cbl", Constants.IO_STANDARD_TEXT_FILE);
	}

	public void testPositionCalc3() throws IOException, RecordException {
		tstPosition("OccursDepending1.cbl", Constants.IO_STANDARD_UNICODE_TEXT_FILE);
	}

	
	private  void tstPosition(String copybookFile, int io)  throws IOException, RecordException {
		String copybookFileName = WriteSampleFile.class.getResource(copybookFile).getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
					.setFileOrganization(io);


		boolean normal = copybookFile.endsWith("1.cbl");
		for (int i = 0; i < 52; i++) {
			for (int j = 0; j < 12; j++) {
				tstLine(ioBuilder.newLine(), i, j, normal);
			}
		}

	}
	

	private void tstLine(AbstractLine line, int purchaseCount, int salesCount, boolean normalPos) throws RecordException {
		LayoutDetail layout = line.getLayout();
		IFieldDetail weekFld = layout.getFieldFromName(WEEK_NO);
		IFieldDetail monthFld = layout.getFieldFromName(MONTHS);
		//IFieldDetail purchCountFld = layout.getFieldFromName("total-purchase-count");
		
		/** Setting the Occurs Depending fields !!! **/
		line.getFieldValue(monthFld).set(salesCount);
		line.getFieldValue(weekFld).set(purchaseCount);
		@SuppressWarnings("deprecation")
		int pos = monthFld.getEnd() + 1;
				
		check(line, layout.getFieldFromName("Location-Number"));
		check(line, layout.getFieldFromName("Location-Name"));

		for (int i = 0; i < salesCount; i++) {
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
			if (salesCount == 7 && i == 5) {
				System.out.print('*');
			}
			pos = check(line, countFld, pos);
			pos = check(line, valueFld, pos);
		}

		if (normalPos) {
			pos = check(line, layout.getFieldFromName("total-sales"), pos);
			pos = check(line, layout.getFieldFromName(WEEK_NO), pos);		
		} else {
			check(line, layout.getFieldFromName("total-sales"));
			check(line, layout.getFieldFromName(WEEK_NO));
		}

		for (int i = 0; i < purchaseCount; i++) {
			pos = check(line, layout.getFieldFromName("purchase-count (" + i + ")"), pos);
			pos = check(line, layout.getFieldFromName("purchase-value (" + i + ")"), pos);
		}

		pos = check(line, layout.getFieldFromName("total-purchase-count"), pos);
		pos = check(line, layout.getFieldFromName("total-purchase-value"), pos);

		
		System.out.println("** line: " + purchaseCount + " " + salesCount + " length=" + line.getData().length);
	}

	private void check(AbstractLine line, IFieldDetail fld) throws RecordException {
		check(line, fld, fld.getPos());
	}
	
	private int check(AbstractLine line, IFieldDetail fld, int pos) throws RecordException {
		String id = fld.getName();
		assertEquals(id, pos, fld.calculateActualPosition(line));
		int end = pos + fld.getLen() - 1;
		assertEquals(id, end, fld.calculateActualEnd(line));
		
		if (WEEK_NO.equalsIgnoreCase(fld.getName()) || MONTHS.equalsIgnoreCase(fld.getName())) {
			
		} else {
			for (int i = 0; i < 4; i++) {
				setAndCheck(line, fld, i);
			}
		}
		return end + 1;
	}
	
	private void setAndCheck(AbstractLine line, IFieldDetail fld, int value) throws RecordException {
		
		AbstractFieldValue fieldValue = line.getFieldValue(fld);
		fieldValue.set(value);
		if (fieldValue.isNumeric()) {
			if (fld.getDecimal() == 0) {
				assertEquals(value, fieldValue.asInt());
			} else {
				assertEquals(Integer.toString(value) + ".00", fieldValue.asString());
			}
		} else {
			assertEquals(Integer.toString(value), fieldValue.asString());
		}
	}

}
