package net.sf.JRecord.Details;

import net.sf.JRecord.Common.Conversion;

public class CharLineProvider implements LineProvider {

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription) {

		return new CharLine(recordDescription, "");
	}

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription, String linesText) {

		return new CharLine(recordDescription, linesText);
	}

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription, byte[] lineBytes) {
		// TODO Auto-generated method stub
		return new CharLine(recordDescription, 
				Conversion.toString(lineBytes, recordDescription.getFontName()));
	}

}
