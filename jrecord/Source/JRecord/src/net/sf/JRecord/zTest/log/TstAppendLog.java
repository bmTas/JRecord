package net.sf.JRecord.zTest.log;

import net.sf.JRecord.Log.AppendableLog;
import junit.framework.TestCase;

public class TstAppendLog extends TestCase {
	String[] lines = {
		"a line",
		"Line  2",
		"Line  3",
		"Line  4",
		"Yet another line",
	};
	public void test01() {
		StringBuilder b = new StringBuilder();
		StringBuilder b1 = new StringBuilder();
		AppendableLog l = new AppendableLog(b);
		
		for (String s : lines) {
			b1.append('\n').append(s).append('\n');
			l.logMsg(0, s);
			assertEquals(b1.toString(), b.toString());
		}
	}
}
