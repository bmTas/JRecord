/**
 * 
 */
package net.sf.JRecord.zTest.Types1;

import java.io.IOException;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalConversion;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.Types.TypeSignSeparate;

/**
 * @author Bruce01
 *
 */
public class WriteTestFiles {
	TestData td;
	
	
	/**
	 * This method creates a Test Data file.
	 * The File created should be checked manually and it can then be used
	 * to run Tests
	 */
	private void createFile(String charset) {
		try {
			td = new TestData(charset);
			String[] values1 = {"0", "5", "10", "32", "432", "6543", "-5", "-10", "-32", "-432", "255", "256", "257",
					"-255", "-256", "-257"};
			String[] values2 = {"1.1", "-2.1", "54.3"};
			AbstractLineWriter w = LineIOProvider.getInstance().getLineWriter(td.testDataDefinition.getFileStructure(), charset);
			AbstractLine l;
	
			TypeManager m = TypeManager.getInstance();
			Type tch = m.getType(Type.ftChar);
			boolean multiByte = Conversion.isMultiByte(charset);
			
			if (multiByte) {
				l = new CharLine(td.getCopybook(charset), "         ");  // Note it does not matter what record-layout is used 
			} else {
				l = new Line(td.getCopybook(charset));					  // Note it does not matter what record-layout is used 
			}
			
			try {
				w.open(TestDataConstants.getTestDataFileName(charset));
				for (int i = 0; i < 200; i++) {
					switch (i) {
					case Type.ftFloat:
					case Type.ftDouble:
					case Type.ftHex:
					case Type.ftBit:
						break;
					case Type.ftCharRestOfRecord:
					case Type.ftCharNullPadded:
					case Type.ftCharNullTerminated:
						break;
					default:
				
						Type type = m.getType(i);
						if ( (  type.isBinary())
						|| (type == tch && i != Type.ftChar)) {	
							
						} else {
							FieldDetail fld1 = TestDataConstants.getType(1, 4, 0, i, charset);
							FieldDetail fld2 = TestDataConstants.getType(1, 4, 1, i, charset);
							
							setValue(w, l, fld1, type, values1);
							
							setValue(w, l, fld2, type, values2);
						}
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
			} catch (RecordException e) {
				e.printStackTrace();
			} finally {		
				w.close();
			}
		} catch (Exception e1) {
			e1.printStackTrace();
		}
	}
	
	/**
	 * Set a field to all the values in an array and write the result to the file
	 * @param writer writer to write the result of the Test to the file
	 * @param line line to be updated
	 * @param fld field to update
	 * @param type Type of the field
	 * @param values values to be used to update the line
	 * @throws RecordException any error from update
	 * @throws IOException any IO error that occurs
	 */
	private void setValue(AbstractLineWriter writer, AbstractLine line, FieldDetail fld, Type type, String[] values) 
	throws RecordException, IOException {
		String s = ExternalConversion.getTypeAsString(0, fld.getType());
		for (String v : values) {
			if ((v.startsWith("-") && (type instanceof TypeNum && ((TypeNum) type).isPositive()))
			|| (v.length() == 4 && (! v.startsWith("-") && type instanceof TypeSignSeparate))
			) {
				
			} else {
				Line outLine = new Line(td.testDataDefinition);	
				AbstractFieldValue fieldValue = line.getFieldValue(fld);
				fieldValue.set(v);
				
				for (IFieldDetail spFld : td.spaceFields) {
					outLine.getFieldValue(spFld).set("");
				}
				outLine.getFieldValue(td.typeDescription).set(s);
				outLine.getFieldValue(td.typeNumber     ).set(fld.getType());
				outLine.getFieldValue(td.fieldLength    ).set(fld.getLen());
				outLine.getFieldValue(td.decimalLength  ).set(fld.getDecimal());
				outLine.getFieldValue(td.testValue      ).set(v);
				outLine.getFieldValue(td.testResult     ).set("");
				outLine.getFieldValue(td.testResultHex  ).set(fieldValue.asHex());
				if (! fieldValue.isBinary()) {
					outLine.getFieldValue(td.testResult)
							.set(line.getFullLine().substring(fld.getPos() - 1, fld.getLen()));
				}
				s = "";
				writer.write(outLine);
			}
		}
	}
	
	/**
	 * This program writes 3 Test files. These files should be checked manually
	 * @param args
	 */
	public static void main(String[] args) {
		
		WriteTestFiles w = new WriteTestFiles();
	
		w.createFile("");
		w.createFile("CP037");
		w.createFile("CP273");

	}

}
