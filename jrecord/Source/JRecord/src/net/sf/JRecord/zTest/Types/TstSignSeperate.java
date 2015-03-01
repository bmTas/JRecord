package net.sf.JRecord.zTest.Types;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeSignSeparate;
import junit.framework.TestCase;

public class TstSignSeperate extends TestCase {

	String[][] VALUES = {
			{"-1", "01-", "-01" },
			{"-12", "12-", "-12" },
			{"1", "01+", "+01" },
			{"12", "12+", "+12" },
	};
	public void testTrailing() throws RecordException {
		int signSepTypeId = Type.ftSignSeparateTrail;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		
		for (String[] vals : VALUES) {
			assertEquals(vals[1], sep.formatValueForRecord(f, vals[0]));
		}
	}
	
	public void testLeading() throws RecordException {
		int signSepTypeId = Type.ftSignSeparateLead;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		
		for (String[] vals : VALUES) {
			assertEquals(vals[2], sep.formatValueForRecord(f, vals[0]));
		}
	}
	

	
	private FieldDetail getField(int type) {
		FieldDetail f = new FieldDetail("", "", type, 0, "", 0, "");
		f.setPosLen(1, 3);
		return f;
	}
}
