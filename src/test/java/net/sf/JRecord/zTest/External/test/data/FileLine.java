package net.sf.JRecord.zTest.External.test.data;

import java.io.IOException;
import java.util.Iterator;

import static org.junit.jupiter.api.Assertions.assertEquals;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;

public class FileLine {
	
	private final int recordIndex;
	private final String[] fileData;
	
	public FileLine(int recordIndex, String... fileData) {
		super();
		this.recordIndex = recordIndex;
		this.fileData = fileData;
	}

	public void checkLine(AbstractLine line) {
		assertEquals(recordIndex, line.getPreferredLayoutIdx());
		
		Iterator<AbstractFieldValue> fieldIterator = line.getFieldIterator();
		int index = 0;
		while (fieldIterator.hasNext()) {
			AbstractFieldValue v = fieldIterator.next();
			String value = v.asString();
			if (fileData[index].isEmpty() && value.isEmpty()) {
				index+=1;
			} else {
				assertEquals(fileData[index++], v.asString());
			}
		}

	}
	
	public static void print(AbstractLineReader reader) throws IOException {
		AbstractLine line;
		
		while( (line = reader.read()) != null) {
			FileLine.printALine(line);
		}
		reader.close();
	}
	
	public static void printALine(AbstractLine line) {
		System.out.print("\t\tnew FileLine(" + line.getPreferredLayoutIdx() + ", ");
		Iterator<AbstractFieldValue> fieldIterator = line.getFieldIterator();
		String sep = "";
		
		while (fieldIterator.hasNext()) {
			AbstractFieldValue v = fieldIterator.next();
			System.out.print(sep + "\"" + v.asString() + "\"");
			sep = ", ";
		}
		System.out.println("),");
	}
}
