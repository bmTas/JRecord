package net.sf.JRecord.zTest.Common;

import java.util.Arrays;

import junit.framework.TestCase;
import net.sf.JRecord.Common.ByteArray;

/**
 * Testing the ByteArray class. The ByteArray class
 * is an extendable byte-array
 * 
 * @author Bruce Martin
 *
 */
public class TstByteArray extends TestCase {

	private static final int ARRAY_INSERT_COUNT = 6;
	
	byte[] expected = {0, 1, 2, 3, 4, };
	public void test01() {
		ByteArray ba = new ByteArray();
		
		for (byte i = 0; i < 5; i++) {
			ba.add(i);
		}
		
		byte[] bytes = ba.toByteArray();
		
		assertEquals(5, bytes.length);
		assertTrue(Arrays.equals(expected, bytes));
		
		int arraySize = (ARRAY_INSERT_COUNT + 1) * expected.length;
		byte[] e = new byte[arraySize];
		int ins = 0;
		
		for (int i = 0; i < ARRAY_INSERT_COUNT; i++) {
			ba.add(expected);
		}
		for (int i = 0; i <= ARRAY_INSERT_COUNT; i++) {
			for (byte b : expected) {
				e[ins++] = b;
			}
		}
		
		bytes = ba.toByteArray();
		
		assertEquals(arraySize, bytes.length);
		assertTrue(Arrays.equals(e, bytes));
	}
}
