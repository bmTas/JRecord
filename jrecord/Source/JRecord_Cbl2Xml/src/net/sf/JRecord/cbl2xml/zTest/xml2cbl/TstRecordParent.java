package net.sf.JRecord.cbl2xml.zTest.xml2cbl;

import static org.junit.Assert.*;
import net.sf.JRecord.cbl2xml.impl.RecordParent;

import org.junit.Test;

/**
 * Test the Record Parent (parameter Class
 * 
 * @author Bruce Martin
 *
 */
public class TstRecordParent {

	@Test
	public void testValid() {
		check(new RecordParent("aa bb"));
		check(new RecordParent(" aa bb "));
		check(new RecordParent("aa  bb"));
		check(new RecordParent(" aa    bb "));
	}
	@Test
	public void testInValid() {
		assertFalse(new RecordParent(" ").ok());
		assertFalse(new RecordParent("aa b c").ok());
		assertFalse(new RecordParent("aa").ok());
		assertFalse(new RecordParent(" aa ").ok());
		assertFalse(new RecordParent(" aa").ok());
		assertFalse(new RecordParent("aa ").ok());
		
	}

	private void check(RecordParent rp) {
		assertTrue(rp.ok());
		assertEquals(rp.recordName, "aa");
		assertEquals(rp.parentName, "bb");
	}
}
