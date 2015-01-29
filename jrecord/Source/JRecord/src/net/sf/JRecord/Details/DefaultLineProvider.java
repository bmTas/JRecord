/*
 * @Author Bruce Martin
 * Created on 29/08/2005
 *
 * Purpose:
 */
package net.sf.JRecord.Details;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;



/**
 * This class returns a Standard <b>Line</b> to the calling program.
 *
 * @author Bruce Martin
 *
 */
public class DefaultLineProvider implements LineProvider {


    /**
     * Build a Line
     *
     * @param recordDescription record layout details
     *
     * @return line just created
     */
    public AbstractLine getLine(LayoutDetail recordDescription) {
		if (recordDescription.isCsvLayout() && CommonBits.useCsvLine()) {
			return new CsvLine(recordDescription);
		}
        return new Line(recordDescription);
    }

    /**
     * Build a Line
     *
     * @param recordDescription record layout details
     * @param linesText text to create the line from
     *
     * @return line
     */
    public AbstractLine getLine(LayoutDetail recordDescription, String linesText) {
		if (recordDescription.isCsvLayout() && CommonBits.useCsvLine()) {
			return new CsvLine(recordDescription, linesText);
		}
        return new Line(recordDescription, linesText);
    }


    /**
     * Build a Line
     *
     * @param recordDescription record layout details
     * @param lineBytes bytes to create the line from
     *
     * @return line
     */
    public AbstractLine getLine(LayoutDetail recordDescription, byte[] lineBytes) {
		if (recordDescription.isCsvLayout() && CommonBits.useCsvLine()) {
			return new CsvLine(recordDescription, Conversion.toString(lineBytes, recordDescription.getFontName()));
		}

        return new Line(recordDescription, lineBytes);
    }
}
