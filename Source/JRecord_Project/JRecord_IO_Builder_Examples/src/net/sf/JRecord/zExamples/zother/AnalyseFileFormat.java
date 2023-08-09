package net.sf.JRecord.zExamples.zother;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map.Entry;

import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.Common.BasicFileSchema;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;

public class AnalyseFileFormat {
	private static ReaderProvider[] IO_TESTERS = {
			new ReaderProvider(IFileStructureConstants.IO_VB_DUMP),
			new ReaderProvider(IFileStructureConstants.IO_VB_DUMP),
			new ReaderProvider(IFileStructureConstants.IO_VB),
			new ReaderProvider(IFileStructureConstants.IO_VB_GNU_COBOL),
			new ReaderProvider(IFileStructureConstants.IO_VB_FUJITSU),
	};
	public static final String TEXT_CHARS = "+-.,abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
											  + "\n\t ;:?{}<>()[]\"'%$";
	public static final boolean[] ASCII_CHECK = getTextChars("");
	public static final boolean[] EBCDIC_CHECK = getTextChars("cp037");
	
	boolean likelyAscii = false, likelyEbcdic = false;
	int fileStructure = -1, recordLength = -1;
	
	public AnalyseFileFormat(LayoutDetail schema, InputStream in) throws IOException {
		byte[] buf = readBuffer(in);
		
		checkEncoding(buf);

		for (ReaderProvider rp : IO_TESTERS ) {
			if (rp.doTest(new ByteArrayInputStream(buf))) {
				fileStructure = rp.fileStructure;
				break;
			}
		}
		checkForTextFile(buf);
	}


	private void checkEncoding(byte[] buf) {
		int countAscii=0, countEbcdic=0;
		
		if (buf.length > 20) {
			for (int i = 0; i < buf.length; i++) {
				countAscii  = ASCII_CHECK[toIndex(buf[i])] ? countAscii + 1 : countAscii;
				countEbcdic = EBCDIC_CHECK[toIndex(buf[i])] ? countEbcdic + 1 : countEbcdic;
			}
			likelyAscii  = countAscii >= countEbcdic && countAscii * 10 > buf.length;
			likelyEbcdic = countAscii <= countEbcdic && countEbcdic * 10 > buf.length;
		}
	}
	
	
	private void checkForTextFile(byte[] buf) throws IOException {
		if (likelyAscii && fileStructure < 0) {
			HashMap<Integer, IntVal> lineLengths = new HashMap<Integer, IntVal>();
			
			BufferedReader r = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(buf)));
			String s;
			int count = 0;
			while ((s = r.readLine()) != null) {
				int length = s.length();
				IntVal v = lineLengths.get(length);
				if (v == null) {
					lineLengths.put(length, new IntVal(length));
				} else {
					v.val += 1;
				}
				count += 1;
			}
			if (count > 20) {
				boolean couldBe = false;
				int lineNum = 0, commonLines = 0, commonCount = 0;;
				
				for (Entry<Integer, IntVal> v : lineLengths.entrySet()) {
					int val = v.getValue().val;
					if (val > 10) {
						commonLines += 1;
						commonCount += val;
						if (val * 6 > count) {
							couldBe = true;
						}
					}
					lineNum += 1;
				}
				
				if (couldBe || commonLines < 10 || (commonCount *10 > lineNum * 7 && commonLines < 20)) {
					fileStructure = IFileStructureConstants.IO_BIN_TEXT;
				}
			}
		}
	}


	
	private void checkForTextFile(LayoutDetail schema, byte[] buf) throws IOException {
		int recLength = schema.getMaximumRecordLength();
		
		
		if (buf.length < recLength * 4) { return; }
		
		byte[] fillBytes = {(byte) 0, (byte) 20, (byte) 40};
		boolean foundFill = false;
		Line line = new Line(schema);
		
		for (byte fill : fillBytes) {
			if (buf[recLength] == fill) {
				int p = recLength+1;
				while (buf[p] == fill) {
					p +=1;
				}
				p -=1;
				
				boolean check = false;
				byte[] data = new byte[p];
				for (int i=0; i < 20 && (i+1)*p <= buf.length; i++) {
					System.arraycopy(buf, i*p, data, 0, p);
					line.setData(data);
					for (int recNo = 0; recNo < schema.getRecordCount(); recNo++) {
						RecordDetail record = schema.getRecord(recNo);
						//for (int fldNo = 0; )
					}
				}
			}
		}
		
	}
	

	private static boolean[] getTextChars(String font) {
		boolean[] isText = new boolean[256];
		try {
			byte[] bytes;
			if (font == null || "".equals(font)) {
				bytes= TEXT_CHARS.getBytes();
			} else {
				bytes= TEXT_CHARS.getBytes(font);
			}
			int j;
			for (int i = 0; i < bytes.length; i++) {
				j = toIndex(bytes[i]);
				isText[j] = true;
	    	}
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		return isText;
	}

	private static int toIndex(byte b) {
		return b + 128;
	}

	public int getFileStructure() {
		return fileStructure;
	}

	protected byte[] readBuffer(InputStream in) throws IOException {
		byte[] buf = new byte[128000];
		int total = 0;
		int num = in.read(buf, total, buf.length - total);

		while (num >= 0 && total + num < buf.length) {
			total += num;
			num = in.read(buf, total, buf.length - total);
		}

		if (num > 0) {
			total += num;
		}
		if (total != buf.length) {
			byte[] tmp = new byte[total];
			System.arraycopy(buf, 0, tmp, 0, total);
			buf = tmp;
		}
		return buf;
	}

//	public static void main(String[] args) {
//		// TODO Auto-generated method stub
//
//	}

	private static class ReaderProvider {
		final int fileStructure;
		final BasicFileSchema schema;
		
		
		public ReaderProvider(int fileStructure) {
			super();
			this.fileStructure = fileStructure;
			this.schema = BasicFileSchema.newFixedSchema(fileStructure);
		}
		
		AbstractByteReader getReader(InputStream in) throws IOException {
			AbstractByteReader r = ByteIOProvider.getInstance().getByteReader(schema);
			r.open(in);
			return r;
		}
		
		boolean doTest(InputStream in) {
			AbstractByteReader reader = null;
					
			try {
				reader = getReader(in);
				int count = 0;
				
				while (count++ < 50 && reader.read() != null) {};
				reader.close();
				return count >= 20;
			} catch (Exception e) {
				return false;
			}
		}
	}
	
	private static class IntVal {
		final int key;
		int val = 1;
		
		public IntVal(int key) {
			super();
			this.key = key;
		}
	}
}
