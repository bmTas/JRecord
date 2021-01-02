/**
 * 
 */
package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;

/**
 * Line-Reader basedon a line of lines
 * @author Bruce Martin
 *
 */
public class ListLineReader extends AbstractLineReader {

	private final List<? extends AbstractLine> lineList;
	private int lineNum = 0;

	
	public ListLineReader(List<? extends AbstractLine> lineList, LayoutDetail layout) {
		super();
		this.lineList = lineList;
		if (layout != null || (lineList != null && lineList.size() > 0)) {
			super.setLayout(layout != null ? layout : lineList.get(0).getLayout());
		}
	}

	@Override
	public void open(InputStream inputStream, LayoutDetail pLayout) throws IOException {
		lineNum = 0;
	}

	@Override
	public AbstractLine readImplementation() throws IOException {
		if (lineList == null || lineNum >= lineList.size()) {
			return null;
		}
		return lineList.get(lineNum++);
	}

	@Override
	public void close() throws IOException {
		lineNum = lineList.size();
	}

}
