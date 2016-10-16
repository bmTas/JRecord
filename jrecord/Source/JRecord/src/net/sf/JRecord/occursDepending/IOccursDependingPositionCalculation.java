package net.sf.JRecord.occursDepending;

import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.Def.DependingOnDtls;

/**
 * This Interface defines a series of <i>Stratergies</i> for
 * calculating field positions when Occurs Depending isd used in Cobol.
 * 
 * @author Bruce Martin
 *
 */
public interface IOccursDependingPositionCalculation {

	public int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos);

	public void checkForSizeFieldUpdate(AbstractLine line, IFieldDetail fld);

	public void clearBuffers(AbstractLine line);
}
