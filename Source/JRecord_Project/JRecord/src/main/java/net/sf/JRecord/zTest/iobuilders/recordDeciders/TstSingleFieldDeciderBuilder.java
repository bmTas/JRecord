package net.sf.JRecord.zTest.iobuilders.recordDeciders;

import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.IRecordDeciderX;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.IO.builders.recordDeciders.SingleFieldDecider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.iobuilders.recordDeciders.CommonDeciderCode.SelValue;

/**
 * Cobol Copybooks do not provide a means to determine which
 * Record applies to a particular Data-Line. One way to tell
 * JRecord / RecordEditor which Record for a Data-line
 * is to define a  {@link RecordDecider} .
 * 
 * <pre>
 *   iobuilder.setRecordDecider(myRecordDecider);
 * </pre
 *
 * JRecord providers a builder to create
 * 
 * @author Bruce Martin
 *
 */
public class TstSingleFieldDeciderBuilder extends TestCase {

	public void testSmall() throws IOException {
		tstDeciderBldr(new CommonDeciderCode.SfDeciderBldr(CommonDeciderCode.DeciderType.SMALL_DECIDER));
	}

	public void testSmallCaseSensitive() throws IOException {
		tstDeciderBldr(new CommonDeciderCode.SfDeciderBldr(CommonDeciderCode.DeciderType.SMALL_CASE_SENSITIVE_DECIDER));
	}

	public void testLarge() throws IOException {
		tstDeciderBldr(new CommonDeciderCode.SfDeciderBldr(CommonDeciderCode.DeciderType.LARGE_DECIDER));
	}

	public void testLargeCaseSensitive() throws IOException {
		tstDeciderBldr(new CommonDeciderCode.SfDeciderBldr(CommonDeciderCode.DeciderType.LARGE_DECIDER));
	}


	public void testJRec1() throws IOException {
		tstDeciderBldr(new CommonDeciderCode.JrDeciderBldr(true));
		tstDeciderBldr(new CommonDeciderCode.JrDeciderBldr(false));
	}
	
	public void testJRec2() throws IOException {
		CommonDeciderCode.JrDeciderBldr jrDeciderBldr = new CommonDeciderCode.JrDeciderBldr(false);
		CommonDeciderCode tst = new CommonDeciderCode();
		
		IRecordDeciderX decider;
		
		decider = jrDeciderBldr.build(null, false, tst.getSelValues(3));
		assertTrue(decider.getClass().getName(), decider instanceof SingleFieldDecider.SmallDecider);
		assertTrue(jrDeciderBldr.getType() == CommonDeciderCode.DeciderType.SMALL_DECIDER);
		
		decider = jrDeciderBldr.build(null, false, tst.getSelValues(11));
		assertTrue(decider.getClass().getName(), decider instanceof SingleFieldDecider.LargeDecider);
		assertTrue(jrDeciderBldr.getType() == CommonDeciderCode.DeciderType.LARGE_DECIDER);
	}

	
	public void testJRec2CaseSensitive() throws IOException {
		CommonDeciderCode.JrDeciderBldr jrDeciderBldr = new CommonDeciderCode.JrDeciderBldr(true);
		CommonDeciderCode tst = new CommonDeciderCode();
		
		IRecordDeciderX decider;
		
		decider = jrDeciderBldr.build(null, false, tst.getSelValues(3));
		assertTrue(decider instanceof SingleFieldDecider.SmallDeciderCaseSensitive);
		assertTrue(jrDeciderBldr.getType() == CommonDeciderCode.DeciderType.SMALL_CASE_SENSITIVE_DECIDER);
		
		decider = jrDeciderBldr.build(null, false, tst.getSelValues(11));
		assertTrue(decider.getClass().getName(), decider instanceof SingleFieldDecider.LargeDecider);
		assertTrue(jrDeciderBldr.getType() == CommonDeciderCode.DeciderType.LARGE_CASE_SENSITIVE_DECIDER);
	}

	void tstDeciderBldr(CommonDeciderCode.ISfDeciderBldr deciderBldr) throws IOException {
		tstDeciderBldr(deciderBldr, 3);
		tstDeciderBldr(deciderBldr, 8);
		tstDeciderBldr(deciderBldr, 12);
		tstDeciderBldr(deciderBldr, 16);
	}
	
	void tstDeciderBldr(CommonDeciderCode.ISfDeciderBldr deciderBldr, int count) throws IOException {
		CommonDeciderCode tst = new CommonDeciderCode();
		
		SelValue[] tstValues = tst.getSelValues(count);
		//List<SelectionValue> selections = tst.getSelectionValues(count);
		ICobolIOBuilder bldr = tst.bldr();
		String defaultRecordName = "Record-B";
		tstDeciderBuilder(deciderBldr, tst, tstValues, bldr, defaultRecordName, false);
		tstDeciderBuilder(deciderBldr, tst, tstValues, bldr, null, false);
		tstDeciderBuilder(deciderBldr, tst, tstValues, bldr, null, true);
	}

	/**
	 * @param deciderBldr
	 * @param tst
	 * @param tstValues
	 * @param bldr
	 * @param defaultRecordName
	 * @throws IOException
	 */
	public void tstDeciderBuilder(CommonDeciderCode.ISfDeciderBldr deciderBldr, CommonDeciderCode tst,
			SelValue[] tstValues, ICobolIOBuilder bldr, String defaultRecordName, boolean allowOtherKeys) throws IOException {
		bldr.setRecordDecider(deciderBldr.build(defaultRecordName, allowOtherKeys, tstValues));
		
		LayoutDetail layout = bldr.getLayout();
		
		for (SelValue val : tstValues) {
			AbstractLine line = bldr.newLine();
			line.getFieldValue(CommonDeciderCode.RECORD_TYPE).set(val.val);
			assertEquals(val.val + " ~ " + val.record, 
					val.record, 
					layout.getRecord(line.getPreferredLayoutIdx()).getRecordName());
		}
		
		switch (deciderBldr.getType()) {
		case LARGE_DECIDER:
		case SMALL_DECIDER:
			for (SelValue val : tstValues) {
				AbstractLine line = bldr.newLine();
				line.getFieldValue(CommonDeciderCode.RECORD_TYPE).set(val.val.toLowerCase());
				assertEquals(val.record, layout.getRecord(line.getPreferredLayoutIdx()).getRecordName());
			}
			break;
			default:
				if (defaultRecordName != null) {
					for (SelValue val : tstValues) {
						AbstractLine line = bldr.newLine();
						line.getFieldValue(CommonDeciderCode.RECORD_TYPE).set(val.val.toLowerCase());
						assertEquals(defaultRecordName, layout.getRecord(line.getPreferredLayoutIdx()).getRecordName());
					}
				} else {
					boolean ok = true;
					for (SelValue val : tstValues) {
						AbstractLine line = bldr.newLine();
						line.getFieldValue(CommonDeciderCode.RECORD_TYPE).set(val.val.toLowerCase());
						if (allowOtherKeys) {
							line.getPreferredLayoutIdx();
						} else {
							try {
								line.getPreferredLayoutIdx();
								ok = false;
								System.out.println("Failed: " + val.val + " ~ " + val.record);
							} catch (Exception e) {
								// TODO: handle exception
							}
						}
					}
					assertTrue(ok);
				} 
		}
	}

}
