package net.sf.JRecord.Details;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

import net.sf.JRecord.Common.Conversion;

public class CsvLine extends ListLine {


	public CsvLine(LayoutDetail layoutDetails, String s) {
		this(layoutDetails);
		
		setData(s);
	}


	public CsvLine(LayoutDetail layoutDetails) {
		super(layoutDetails);
		
		if (! layoutDetails.isCsvLayout()) {
			throw new RuntimeException("Layout must be a CsvLayout !!!");
		}
	}

	@Override
	public byte[] getData() {
		String s = getFullLine();
		byte[] b;
		
		if ("".equals(layout.getFontName())) {
			b = s.getBytes();
		} else {
			try {
				b = s.getBytes(layout.getFontName());
			} catch (UnsupportedEncodingException e) {
				throw new RuntimeException(e);
			}
		}
		return b;
	}


	@Override
	public String getFullLine() {
		RecordDetail record = layout.getRecord( getPrefIdx());
		
		return record
				.getParser()
				.formatFieldList(
						fields, record, record.getFieldTypes());
	}
	
	public void setData(byte[] newVal) {
		setData(Conversion.toString(newVal, layout.getFontName()));
	}
	
	@Override
	public void setData(String newVal) {
		RecordDetail record = layout.getRecord(getPrefIdx());

		fields = new ArrayList<Object>(
				record.getParser().getFieldList(newVal, record));
	}

	
	

	@Override
	protected final int getAdj() {
		return 1;
	}


	private int getPrefIdx() {
		int idx = getPreferredLayoutIdx();
		if (idx < 0 || idx >= layout.getRecordCount()) {
			idx = 0;
		}
		return idx;
	}
}
