package net.sf.JRecord.Details;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;

public class CharLineProvider implements LineProvider {

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription) {

		if (recordDescription.isCsvLayout() && CommonBits.useCsvLine()) {
			return new CsvLine(recordDescription);
		}
		return new CharLine(recordDescription, "");
	}

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription, String linesText) {

		if (recordDescription.isCsvLayout() && CommonBits.useCsvLine()) {
			return new CsvLine(recordDescription, linesText);
		}
		return new CharLine(recordDescription, linesText);
	}

	@Override
	public AbstractLine getLine(LayoutDetail recordDescription, byte[] lineBytes) {
		// TODO Auto-generated method stub
		return getLine(recordDescription, 
				Conversion.toString(lineBytes, recordDescription.getFontName()));
	}

}
