package net.sf.JRecord.ByteIO;

import java.math.BigInteger;

import net.sf.JRecord.Common.Conversion;

public final class MicroFocusFileHeader {
	public static final int MIN_4_BYTE_LENGTH = 4095;
	
	public static final byte FORMAT_SEQUENTIAL = 1; 
	public static final byte FORMAT_INDEXED = 2; 
	public static final byte FORMAT_RELATIVE = 3; 
	
	public static final byte RECORD_FIXED_LENGTH = 0; 
	public static final byte RECORD_VARIABLE_LENGTH = 1; 

	public static final byte[] DATE_TIME = {48, 48, 48, 49, 48, 49, 48, 48, 48, 48, 48, 48, 48, 48};
	
	private final byte[] headerRec;
	
	private final int minLength, maxLength;


	public MicroFocusFileHeader(byte[] headerRecord) {

		this.headerRec = headerRecord;
		maxLength = getLength(54);
		minLength = getLength(58);
	}
	
	public MicroFocusFileHeader(int org, int minRecLength, int maxRecLength) {
		int i;
		byte[] tmp = new byte[128];
		
		
		
		for (i = 2; i < 128; i++) {
			tmp[i] = 0;	
		}

		tmp[0] = 48;
		if (maxRecLength < MIN_4_BYTE_LENGTH) {
			tmp[1] = 126;
		} else {
			tmp[3] = 124;
		}
//
//		for (i = 0; i < DATE_TIME.length; i++) {
//			tmp[8 + i] = DATE_TIME[i];
//			tmp[22 + i] = DATE_TIME[i];
//		}
		
		tmp[37] = 62;
		tmp[39] = (byte) org;
		
//		tmp[108] = 65;
//		tmp[110] = 25;
//		tmp[111] = -68;
		
		try {
			Conversion.setLong(tmp, 54, 4, maxRecLength, true);
			Conversion.setLong(tmp, 58, 4, minRecLength, true);
		} catch (Exception e) {
			System.out.println("Error Setting Min max length in Microfocus Header");
		}
		
//		if (minRecLength != maxRecLength) {
			tmp[48] = RECORD_VARIABLE_LENGTH;
//		}
		
		minLength = minRecLength;
		maxLength = maxRecLength;
		headerRec = tmp;
	}
	
	private int getLength(int start) {
		byte[] len = new byte[4];
		
		System.arraycopy(headerRec, start, len, 0, 4);
		return new BigInteger(len).intValue();
	}

	public byte[] getHeaderRec() {
		return headerRec;
	}
	
	public byte getFileFormat() {
		return headerRec[39];
	}
	
	
	public byte getCompression() {
		return headerRec[41];
	}
	
	
	public boolean isValidFile() {
		return headerRec[6] == 0 && headerRec[7] == 0;
	}
	
	public byte getRecordFormat() {
		return headerRec[48];
	}

	public int getMinLength() {
		return minLength;
	}

	public int getMaxLength() {
		return maxLength;
	}
}
