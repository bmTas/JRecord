package net.sf.JRecord.zTest.Types1;

import java.math.BigDecimal;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.Types.TypeZoned;
import net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric;
import net.sf.JRecord.Types.smallBin.TypeZonedAsciiSmall;
import net.sf.JRecord.Types.smallBin.TypeZonedEbcdicSmall;

public class CheckTextPerformance {

	private static final int COUNT = 50000;

	private static String[] zonedValues = {
		"00012C",		"45678J",
		"00001k",		"00001A",
		"10012C",		"42678J",
		"00301k",		"00041A",
		"00032C",		"45638J",
		"00031k",		"10031A",
		"20032C",		"35638J",
		"40031k",		"33445A",
	};
	

	private void doCheck(String id, String charset, TypeNum typeN, ITypeBinaryExtendedNumeric newType) {
		byte[][] lines = new byte[zonedValues.length][];
		int idx = 0;
		for (String s : zonedValues) {
			lines[idx++] = Conversion.getBytes(s, charset);
		}
		
		FieldDetail f = FieldDetail.newFixedWidthField("", typeN.getFieldType(), 1, zonedValues[0].length(), 2, charset);
		System.out.println("\n\n" + id + "\n");
	
		chkOldBd(typeN, lines, f);
		checkNewBd(newType, lines, f);
		chkOldDouble(typeN, lines, f);
		checkNewDouble(newType, lines, f);
		chkOldString(typeN, lines, f);
		checkNewString(newType, lines, f);

		
		long cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) chkOldBd(typeN, lines, f);
		System.out.println("Old PD Time (old): " + (System.currentTimeMillis() - cpuTime));
		
		cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) checkNewBd(newType, lines, f);
		System.out.println("New PD Time (new): " + (System.currentTimeMillis() - cpuTime));
				
		cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) chkOldDouble(typeN, lines, f);
		System.out.println("Old Double Time: " + (System.currentTimeMillis() - cpuTime));
		
		cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) checkNewDouble(newType, lines, f);
		System.out.println("New Double Time: " + (System.currentTimeMillis() - cpuTime));
		
		cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) chkOldString(typeN, lines, f);
		System.out.println("Old String Time: " + (System.currentTimeMillis() - cpuTime));
		
		cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) checkNewString(newType, lines, f);
		System.out.println("New String Time: " + (System.currentTimeMillis() - cpuTime));
		

		cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) checkSet(typeN,  f);
		System.out.println("Old Double Set Time: " + (System.currentTimeMillis() - cpuTime));
		
		cpuTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) checkSet(newType,  f);
		System.out.println("New Double Set Time: " + (System.currentTimeMillis() - cpuTime));
	}

	private void checkSet(Type typeN, FieldDetail f) {
		byte[] record = {0, 0, 0, 0, 0, 0, 0, 0};
		double[] dbls = {
				123.45, 4565, 2345.44, 1.23, 23, 456, 0, 1, 121
		};
		for (int i = 0; i < COUNT; i++) {
			for (double d : dbls) {
				typeN.setField(record, 1, f, d);
			}
		}
	}


	/**
	 * @param newType
	 * @param lines
	 * @param f
	 */
	private void checkNewBd(ITypeBinaryExtendedNumeric newType, byte[][] lines, FieldDetail f) {
		for (int i = 0; i < COUNT; i++) {
			for (byte[] rec : lines) {
				//System.out.print("\t" + Conversion.toString(rec, "cp037"));
				BigDecimal bd = BigDecimal.valueOf(newType.asUnscaledLong(rec, 1, f), 2);
			}
		}
	}


	/**
	 * @param typeN
	 * @param lines
	 * @param f
	 */
	private void chkOldBd(TypeNum typeN, byte[][] lines, FieldDetail f) {
		for (int i = 0; i < COUNT; i++) {
			for (byte[] rec : lines) {
				String string = typeN.getField(rec, 1, f).toString();
				BigDecimal bd = new BigDecimal(
						string
				);
			}
		}
	}
	
	/**
	 * @param newType
	 * @param lines
	 * @param f
	 */
	private void checkNewDouble(ITypeBinaryExtendedNumeric newType, byte[][] lines, FieldDetail f) {
		for (int i = 0; i < COUNT; i++) {
			for (byte[] rec : lines) {
				double bd = newType.asUnscaledLong(rec, 1, f) / 100;
			}
		}
	}


	/**
	 * @param typeN
	 * @param lines
	 * @param f
	 */
	private void chkOldString(TypeNum typeN, byte[][] lines, FieldDetail f) {
		for (int i = 0; i < COUNT; i++) {
			for (byte[] rec : lines) {
				String s = typeN.getField(rec, 1, f).toString();
			}
		}
	}
	
	/**
	 * @param newType
	 * @param lines
	 * @param f
	 */
	private void checkNewString(ITypeBinaryExtendedNumeric newType, byte[][] lines, FieldDetail f) {
		for (int i = 0; i < COUNT; i++) {
			for (byte[] rec : lines) {
				String bd = BigDecimal.valueOf(newType.asUnscaledLong(rec, 1, f), 2).toString();
			}
		}
	}


	/**
	 * @param typeN
	 * @param lines
	 * @param f
	 */
	private void chkOldDouble(TypeNum typeN, byte[][] lines, FieldDetail f) {
		for (int i = 0; i < COUNT; i++) {
			for (byte[] rec : lines) {
				double bd = Double.parseDouble(
						typeN.getField(rec, 1, f).toString()
				);
			}
		}
	}
	
	public static void p(char c) {
		System.out.println(c + "\t: " + (c >> 4) + " " + ((byte) (c + 0)));
	}

	public static void main(String[] args) {
		CheckTextPerformance c = new CheckTextPerformance();
		//p('a'); p('A'); p('{'); p('j'); p('J');
		System.out.println("\n");
		
		c.doCheck("Zoned Ebcdic", "cp037", new TypeZoned(), new TypeZonedEbcdicSmall(false));
		System.out.println("\n");
		c.doCheck("Zoned Ascii", "", new TypeZoned(), new TypeZonedAsciiSmall(false));
		
	}

}
