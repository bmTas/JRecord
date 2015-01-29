package net.sf.JRecord.zTest.Types1;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import junit.framework.TestCase;

/**
 * This class test type updates by reading through a file,
 * applying a value and checking the value against an expected value
 * 
 * @author Bruce Martin
 *
 */
public class TestTypeUpdates extends TestCase {

	public void testTypesStandardCharset() throws Exception {
		tstFile("");
	}

	public void testTypesCP037() throws Exception {
		tstFile("CP037");
	}
	

	public void testTypesCP273() throws Exception {
		tstFile("CP273");
	}
	

	private void tstFile(String charset) throws Exception {
		TestData td = new TestData(charset);
		AbstractLineReader r = LineIOProvider.getInstance().getLineReader(td.testDataDefinition);
		AbstractLine inLine;
		AbstractLine testLine = new Line(td.testDataDefinition);
		
		r.open(TestDataConstants.getTestDataFileName(charset), td.testDataDefinition);
		try {
			while ((inLine = r.read()) != null) {
				int fldLength = inLine.getFieldValue(td.fieldLength).asInt();
				IFieldDetail fld = TestDataConstants.getType(
								1, 
								fldLength, 
								inLine.getFieldValue(td.decimalLength).asInt(), 
								inLine.getFieldValue(td.typeNumber).asInt(), 
								charset);
				AbstractFieldValue fieldValue = testLine.getFieldValue(fld);
				fieldValue.set(inLine.getFieldValue(td.testValue).asString());
				assertEquals("Type 1: " + fld.getType(), inLine.getFieldValue(td.testResultHex).asString(), fieldValue.asHex());
				
				if (! fieldValue.isBinary()) {
//					assertEquals("Type 2: " + fld.getType(), 
//							inLine.getFieldValue(td.testResult).asString(), testLine.getFullLine().substring(0, fldLength));
				}
			}
		} finally {
			r.close();
		}
	}
}
