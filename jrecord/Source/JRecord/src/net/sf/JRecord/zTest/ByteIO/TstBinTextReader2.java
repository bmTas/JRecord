package net.sf.JRecord.zTest.ByteIO;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import net.sf.JRecord.ByteIO.ByteTextReader;

import junit.framework.TestCase;

public class TstBinTextReader2 extends TestCase {
	String[] testData = {
			"",
			"112233",
			"1122\n",
			"1122\n3344",
			"1122\n3344\n",
			"1122\n3344\n5566",
	};
	String[][] testResult = {
			{""},
			{"112233"},
			{"1122", ""},
			{"1122", "3344"},
			{"1122", "3344", ""},
			{"1122", "3344", "5566"},
	};

	public void testReader() throws IOException {
		for (int i = 0; i < testData.length; i++) {
			oneTstCase(i, testData[i], testResult[i]);
		}
	}


	private void oneTstCase(int i, String input, String[] expected) throws IOException {
		ByteArrayInputStream in = new ByteArrayInputStream(input.getBytes());
		ByteTextReader r = new ByteTextReader();
		byte[] b;
		int j = 0;


		r.open(in);
		try {
			while ((b = r.read()) != null) {
				assertEquals("Check: " + i + ",  " + j, expected[j++], new String(b));
			}
		} finally {
			r.close();
		}

	}
}
