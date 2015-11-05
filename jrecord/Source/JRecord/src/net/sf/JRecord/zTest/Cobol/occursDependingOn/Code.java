package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.math.BigDecimal;

import junit.framework.TestCase;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;

public class Code {
	private static final BigDecimal BD_1p1 = new BigDecimal("1.1");
	private static final BigDecimal BD_3p1 = new BigDecimal("3.1");
	private static final BigDecimal BD_5p1 = new BigDecimal("5.1");

	
	
	public static AbstractLine generateLine(AbstractLine line, int purchaseCount, int salesCount) throws RecordException {
		generateSalesLine(line, purchaseCount, salesCount, true);
	
		int count = (purchaseCount * salesCount * 2) % 100;
		line.getFieldValue("total-purchase-count").set(count);
		BigDecimal expected = BigDecimal.valueOf(count).multiply(BD_5p1).setScale(2, BigDecimal.ROUND_HALF_UP);
		line.getFieldValue("total-purchase-value").set(expected); 

		for (int i = 0; i < purchaseCount; i++) {
//			System.out.println("  purch: " + purchaseCount + " " + salesCount + " " + i);
			line.getFieldValue("purchase-count (" + i + ")").set(i + salesCount);
			line.getFieldValue("purchase-value (" + i + ")").set((i +salesCount) * 1.1);
		}
		
//		System.out.println("** line: " + purchaseCount + " " + salesCount + " length=" + line.getData().length);
		return line;
	}


	/**
	 * @param line
	 * @param purchaseCount
	 * @param salesCount
	 * @param layout
	 */
	public static AbstractLine generateSalesLine(AbstractLine line, int purchaseCount,
			int salesCount, boolean writeValue) {
		LayoutDetail layout = line.getLayout();
		line.getFieldValue("months").set(salesCount);     // month should be set first
		line.getFieldValue("week-no").set(purchaseCount); // before week-no because week-no's position is determined by months value !!
		
//		System.out.println("  week pos: " + purchaseCount + " " + salesCount + "  " 
//				+ " " + weekFld.calculateActualPosition(line)
//				+ " " + purchCountFld.calculateActualPosition(line));
		
		line.getFieldValue("Location-Number").set(salesCount * 100 + purchaseCount);
		line.getFieldValue("Location-Name").set("Store " + salesCount + ", " + purchaseCount);

		line.getFieldValue("total-sales").set(salesCount * 100 + purchaseCount);
	
		for (int i = 0; i < salesCount; i++) {
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
	
//			if (salesCount == 7 && i == 5) {
//				System.out.print('*');
//			}
//			System.out.println("  sales: " + purchaseCount + " " + salesCount + " " + i
//					+ " " + weekFld.calculateActualPosition(line)
//					+ " " + countFld.calculateActualPosition(line)
//					+ " " + valueFld.calculateActualPosition(line)
//					+ " " + (valueFld.calculateActualPosition(line) + valueFld.getLen() - 1));
			line.getFieldValue(countFld).set(i + purchaseCount);
			if (writeValue) {
				IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
				line.getFieldValue(valueFld).set((i + purchaseCount) * 3.1);
			}
		}
		
		return line;
	}
	
	
	public static void checkLine(AbstractLine line, int purchaseCount, int salesCount) throws RecordException {
		String storeName = "Store " + salesCount + ", " + purchaseCount;
		System.out.println("** line: " + purchaseCount + " " + salesCount + " length=" + line.getData().length 
				+ " " + storeName
				+ " " + salesCount + " ~ " + line.getFieldValue("months").asInt()
				+ " " + purchaseCount + " ~ " + line.getFieldValue("week-no").asInt()
		);

		checkSalesRecord(line, purchaseCount, salesCount, true);

		int count = (purchaseCount * salesCount * 2) % 100;
		TestCase.assertEquals(count, line.getFieldValue("total-purchase-count").asInt());
		
		BigDecimal expected = BigDecimal.valueOf(count).multiply(BD_5p1).setScale(2, BigDecimal.ROUND_HALF_UP);
		System.out.println(expected + " " + count + " " + (count * 5.1) + " " );
		TestCase.assertEquals(expected, line.getFieldValue("total-purchase-value").asBigDecimal()); 
		for (int i = 0; i < purchaseCount; i++) {
			TestCase.assertEquals(i + salesCount, line.getFieldValue("purchase-count (" + i + ")").asInt());
			TestCase.assertEquals(
						BigDecimal.valueOf(i + salesCount).multiply(BD_1p1).setScale(2),
						line.getFieldValue("purchase-value (" + i + ")").asBigDecimal());
			
        }
		
	}


	/**
	 * @param line
	 * @param purchaseCount
	 * @param salesCount
	 * @param layout
	 * @param storeName
	 */
	public static void checkSalesRecord(AbstractLine line, int purchaseCount,
			int salesCount, boolean checkSalesValue) {
		LayoutDetail layout = line.getLayout();
		String storeName = "Store " + salesCount + ", " + purchaseCount;
		TestCase.assertEquals(salesCount, line.getFieldValue("months").asInt());
		TestCase.assertEquals(purchaseCount, line.getFieldValue("week-no").asInt()); 

		TestCase.assertEquals(salesCount * 100 + purchaseCount, line.getFieldValue("Location-Number").asInt());
		TestCase.assertEquals(storeName, line.getFieldValue("Location-Name").asString());

		TestCase.assertEquals(salesCount * 100 + purchaseCount, line.getFieldValue("total-sales").asBigDecimal().intValue());

		for (int i = 0; i < salesCount; i++) {
			IFieldDetail countFld = layout.getFieldFromName("sales-count (" + i + ")");
			
			if (salesCount == 7 && i == 5) {
				System.out.print('*');
			}

			TestCase.assertEquals(i + purchaseCount, line.getFieldValue(countFld).asInt());
			if (checkSalesValue) {
				IFieldDetail valueFld = layout.getFieldFromName("sales-value (" + i + ")");
				TestCase.assertEquals(
						BigDecimal.valueOf(i + purchaseCount).multiply(BD_3p1).setScale(2), 
						line.getFieldValue(valueFld).asBigDecimal());
			}
		}
	}


}
