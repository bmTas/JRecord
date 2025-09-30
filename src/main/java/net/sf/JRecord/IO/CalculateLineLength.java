package net.sf.JRecord.IO;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;

public class CalculateLineLength implements ICalculateLineLength {
	private int lineNumber;
	private long totalBytesRead;
	
	private final String postionText;
	
	public CalculateLineLength(String postionText) {
		super();
		this.postionText = postionText;
	}

	@Override
	public void setFilePosition(int lineNumber, long totalBytesRead) {
		this.lineNumber = lineNumber;
		this.totalBytesRead = totalBytesRead;
	}
	
	/**
	 * work out the record length
	 *
	 * @param maxLength length of the buffer
	 *
	 * @return the length of the next length
	 */
	@Override
	public int findLength(AbstractLine tmpLine, int maxLength) {
		int pref = tmpLine.getPreferredLayoutIdxAlt();
		
		if (pref < 0) {
		    throw new RuntimeException("Can Not Determine Record Type: " + tmpLine.getFullLine());
		}

		LayoutDetail layout = tmpLine.getLayout();
		RecordDetail rec = layout.getRecord(pref);
		if (rec.hasDependingOn()) {
			FieldDetail f =  rec.getField(rec.getFieldCount() - 1);
			int len;
			try {
				len = rec.calculateActualPosition(tmpLine, f.getDependingOnDtls(), f.getEnd() + 1) - 1;
			} catch (RecordException e) {
				RuntimeException err = new RuntimeException(
						e.getMessage()
						+ "\nCurrent Line Number: " + lineNumber
						+ "\n   Total " + postionText + " Read: " + totalBytesRead);
				err.setStackTrace(e.getStackTrace());
				throw err;
			}
			return len;
		}
		return layout.getRecord(pref).getLength();
	}

}
