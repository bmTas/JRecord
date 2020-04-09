package net.sf.JRecord.zTest.Types1;

import java.io.IOException;
import java.math.BigDecimal;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeBinBigEndian;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.Types.TypePackedDecimal;
import net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric;
import net.sf.JRecord.Types.smallBin.TypeIntBigEndian;
import net.sf.JRecord.Types.smallBin.TypePackedDecimal9;

/**
 * Testing the performance of new Packed Decimal Class
 * @author Bruce Martin
 *
 */
public class CheckBinPerformance {
	
/*
   Test Results
  *------------*

 Packed Decimal Test:

New PD: 35
Old PD: 1087
New Double: 32
Old Double: 1112


Integer Big Endian Test:

New PD: 25
Old PD: 1130
New Double: 19
Old Double: 1198


Integer Little Endian Test:

New PD: 25
Old PD: 1125
New Double: 19
Old Double: 1199

 -------------------------------

 */

	private static final int COUNT = 100000;

	byte[][] byteRecords = {
			{0x12, 0x34, 0x12, 0x1c},
			{0, 0, 0x12, 0x1c},
			{0x34, 0x12, 0x45, 0x1c},
			{0, 0, 0x47, 0x1c},
			{0x39, 0x10, 0x49, 0x1c},
			{0, 9, 0x49, 0x1c},
			{0, 8, 0x19, 0x1c},
			{0x19, 0x37, 0x47, 0x3c},
	};
	FieldDetail field = FieldDetail.newFixedWidthField("", Type.ftPackedDecimal, 1, 4, 2, "");
	
	public void chkStdPdBigDecimal(TypeNum pd) {

		for (int i = 0; i < COUNT; i++) {
			for (byte[] l : byteRecords) {
				BigDecimal bd = new BigDecimal(pd.getField(l, 1, field).toString());
			}
		}
	}

	
	public void chkPd9BigDecimal(ITypeBinaryExtendedNumeric pd) {

		for (int i = 0; i < COUNT; i++) {
			for (byte[] l : byteRecords) {
				BigDecimal bd =  BigDecimal.valueOf(pd.asUnscaledLong(l, 1, field), 2);
			}
		}
	}
	
	
	public void chkStdPdDouble(TypeNum pd) {

		for (int i = 0; i < COUNT; i++) {
			for (byte[] l : byteRecords) {
				double bd = Double.parseDouble(pd.getField(l, 1, field).toString());
			}
		}
	}

	
	public void chkPd9Double(ITypeBinaryExtendedNumeric pd) {

		for (int i = 0; i < COUNT; i++) {
			for (byte[] l : byteRecords) {
				double bd =  pd.asUnscaledLong(l, 1, field) / 100;
			}
		}
	}
	
	public void chkLineGetPd(int type) throws IOException {
		Line[] lines = createLineArray(type);
		
		for (int i = 0; i < COUNT; i++) {
			for (Line l : lines) {
				BigDecimal bd = l.getFieldValue(0, 0).asBigDecimal();
			}
		}
	}
	public void chkLineGetDouble(int type) throws IOException {
		Line[] lines = createLineArray(type);
		
		for (int i = 0; i < COUNT; i++) {
			for (Line l : lines) {
				double bd = l.getFieldValue(0, 0).asDouble();
			}
		}
	}

	public void chkLineGetLong(int type) throws IOException {
		Line[] lines = createLineArray(type);
		
		for (int i = 0; i < COUNT; i++) {
			for (Line l : lines) {
				long bd = l.getFieldValue(0, 0).asLong();
			}
		}
	}

	/**
	 * @param type
	 * @throws IOException
	 */
	private Line[] createLineArray(int type) throws IOException {
		LayoutDetail schema = JRecordInterface1.FIXED_WIDTH
				.newIOBuilder()
					.defineFieldsByLength()
						.addFieldByLength("Field 1", type, 4, 2)
					.endOfRecord()
				.getLayout();
		Line[] lines = new Line[byteRecords.length]; 
		int idx = 0;
		for (byte[] l : byteRecords) {
			lines[idx++] = new Line(schema, l);
		}
		
		return lines;
	}

	public void doFieldValueTst(int type, String name) throws IOException {
		chkLineGetPd(type);
		chkLineGetDouble(type);
		chkLineGetLong(type);
		
		long cpu;

		System.out.println("\n\n" + name + ":\n");

		cpu = System.currentTimeMillis();
		for (int i = 0; i < 5; i++) {
			chkLineGetPd(type);
		}
		System.out.println(name + " PD: " + (System.currentTimeMillis() - cpu));
		
		cpu = System.currentTimeMillis();
		for (int i = 0; i < 5; i++) {
			chkLineGetDouble(type);
		}
		System.out.println(name + " Double: " + (System.currentTimeMillis() - cpu));

		cpu = System.currentTimeMillis();
		for (int i = 0; i < 5; i++) {
			chkLineGetLong(type);
		}
		System.out.println(name + " Long: " + (System.currentTimeMillis() - cpu));
	}
	
	public void doTypeTest(String text, TypeNum pd, ITypeBinaryExtendedNumeric pdNew) {
		
		System.out.println("\n\n" + text + "\n");
		
		
		chkStdPdBigDecimal(pd);
		chkPd9BigDecimal(pdNew);
		chkStdPdBigDecimal(pd);
		chkPd9BigDecimal(pdNew);
		
		long cpu;
		cpu = System.currentTimeMillis();
		for (int i = 0; i < 5; i++) {
			chkPd9BigDecimal(pdNew);
		}
		System.out.println("New PD: " + (System.currentTimeMillis() - cpu));
		
		cpu = System.currentTimeMillis();
		for (int i = 0; i < 5; i++) {
			chkStdPdBigDecimal(pd);
		}
		System.out.println("Old PD: " + (System.currentTimeMillis() - cpu));
		
		chkStdPdDouble(pd);
		chkPd9Double(pdNew);
		chkStdPdDouble(pd);
		chkPd9Double(pdNew);
		
		cpu = System.currentTimeMillis();
		for (int i = 0; i < 5; i++) {
			chkPd9Double(pdNew);
		}
		System.out.println("New Double: " + (System.currentTimeMillis() - cpu));
		
		cpu = System.currentTimeMillis();
		for (int i = 0; i < 5; i++) {
			chkStdPdDouble(pd);
		}
		System.out.println("Old Double: " + (System.currentTimeMillis() - cpu));

	}

	public static void main(String[] args) throws IOException {
		
		double d = 4.9;
		
		System.out.println(" --> " + ((long) d) + " " + ((long) (-d))
				 + " ~~ " + ((long) (d + 0.5)) + " " + ((long) (-d - 0.5))
		);
				
		CheckBinPerformance c = new CheckBinPerformance();
		
		c.doTypeTest("Packed Decimal Test:", new TypePackedDecimal(), new TypePackedDecimal9());
		c.doTypeTest("Integer Big Endian Test:",    new TypeBinBigEndian(false), new TypeIntBigEndian(false, false));
		c.doTypeTest("Integer Little Endian Test:", new TypeBinBigEndian(false), new TypeIntBigEndian(false, false));

		
		
		c.doFieldValueTst(Type.ftPackedDecimal,  "Packed Decimal");
		c.doFieldValueTst(Type.ftPackedDecimalSmall, "New Packed Decimal");
	
		c.doFieldValueTst(Type.ftBinaryBigEndian,  "Int BE");
		c.doFieldValueTst(Type.ftIntBigEndianSmall, "New Int BE");
		
		c.doFieldValueTst(Type.ftBinaryInt,  "Int LE");
		c.doFieldValueTst(Type.ftIntSmall, "New Int LE");
	}

}
