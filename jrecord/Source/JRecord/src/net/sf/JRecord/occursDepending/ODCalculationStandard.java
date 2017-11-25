package net.sf.JRecord.occursDepending;

import java.util.List;

import net.sf.JRecord.Common.AbstractIndexedLine;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.DependingOnDefinition;
import net.sf.JRecord.External.Def.DependingOnDtls;


/**
 * This class will calculate position adjustments for 
 * Standard Occurs Depending (i.e. Occurs Depending
 * size fields are not in arrays). 
 * 
 * @author Bruce Martin
 *
 */
public class ODCalculationStandard implements IOccursDependingPositionCalculation {

//	private final ODCalculationStandard stdCalc;
	private final DependingOnDefinition dependingOn;
	
	
	public ODCalculationStandard(DependingOnDefinition dependingOn) {
		super();
		this.dependingOn = dependingOn;
	}

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.Common.AbstractRecord#calculateActualPosition(net.sf.JRecord.Common.AbstractIndexedLine, net.sf.JRecord.External.Def.DependingOnDtls, int)
//	 */
//	@Override
//	public int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos) {
//		DependingOnDtls[] tree = null;
//		if (dependingOnDtls != null) {
//			tree = dependingOnDtls.getTree();
//		}
//		return pos - calculateAdjustment(dependingOn, line, tree, 0, pos);
//	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.IOccursDependingPositionCalculation#calculateActualPosition(net.sf.JRecord.Common.AbstractIndexedLine, net.sf.JRecord.External.Def.DependingOnDtls, int)
	 */
	@Override
	public int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos) {
		
		return pos - calcAdjustment(dependingOn.dependOnList, line, dependingOnDtls, pos); 
		
	}
	
	private int calcAdjustment(
			List<DependingOn> dependingOnList, final AbstractIndexedLine line, 
			DependingOnDtls dependingOnDtls,
			final int pos) {
		

		DependingOnDtls[] tree = null;
		if (dependingOnDtls != null) {
			tree = dependingOnDtls.getTree();			
		}
		
		return calculateAdjustment(dependingOnList, line, tree, 0, pos);
	}

	/**
	 * Calculate an adjustment to the record Position based on 
	 * 
	 * @param dependingOnList the list of Depending on clauses to use in the calculation
	 * @param line line or record for which to calculate the position
	 * @param dependingOnDtls depending on details for the field which we are calculating the adjustment for
	 * @param lvl current level (or index in dependingOnDtls array 
	 * @param pos position of the field
	 * 
	 * @return Adjustment to be made to the field
	 */
	private int calculateAdjustment(List<DependingOn> dependingOnList, final AbstractIndexedLine line, DependingOnDtls[] dependingOnDtls,
			int lvl, final int pos) {
		if (dependingOnList == null || dependingOnList.size() == 0 || pos < dependingOnList.get(0).getPosition()) {
			return 0;
		}

		int tmpAdj = 0;
		
		for (int i = 0; i < dependingOnList.size() && pos >= dependingOnList.get(i).getPosition(); i++) {
			DependingOn dependingOnDef = dependingOnList.get(i);
			IFieldDetail field = dependingOnDef.getField();	

			int adj = 0;
			try {
				Object value = line.getField(field);
				List<DependingOn> children = dependingOnDef.getChildren();
				int actualOccurs = Integer.parseInt(value.toString().trim());
				int occursLength = dependingOnDef.getOccursLength();

				int childAdjustment = calculateAdjustment(children, line, dependingOnDtls, lvl + 1, pos);
				if (pos > dependingOnDef.getEnd()) {
					int occursMaxLength = dependingOnDef.getOccursMaxLength();
					int actualOccursLength = occursLength - childAdjustment; 
					int actualLength = actualOccurs * actualOccursLength;
	//						- calculateAdjustment(dependingOnDef.getChildren(), line, pos);
					adj = occursMaxLength - actualLength;//calculateAdjustment(dependingOnDef.getChildren(), line, pos);
					if (pos - adj < dependingOnDef.getPosition() + actualLength) {
						return tmpAdj;
					}
				} else if (children != null && children.size() > 0 && childAdjustment > 0) { 
					if (dependingOnDtls != null
					&& lvl < dependingOnDtls.length ) {
						int idx = dependingOnDtls[lvl].index;
						adj = childAdjustment * (idx + 1);
						
						DependingOn c = children.get(children.size() - 1);	

						if (pos < c.getEnd() + idx * occursLength) {
							adj = childAdjustment;
							int occurs = 1;
//							System.out.print("\t$$ " + pos + " - " + c.getEnd() + " > " + occursLength);
							if (occursLength != 0 && pos - c.getEnd() > occursLength) {
								occurs = ((int) (pos - c.getEnd()) / occursLength) + 1;
								if (occurs > 1) {
									adj = childAdjustment * occurs;											
								}
							}
							
							if (pos - children.get(0).getPosition() + children.get(0).getOccursLength() > occursLength) {
								int tChildAdj = calculateAdjustment(children, line, dependingOnDtls, lvl + 1, pos - occurs * occursLength);	
								adj += tChildAdj;						
							}
						}
					} else {
						adj = childAdjustment;
					}
				}
				tmpAdj += adj;
			} catch (RuntimeException e) {
				System.out.println();
				System.out.println("ODCalculationStandard ~ Error Retrieving: " + (field==null?"null field":field.getName()));
				System.out.println();
				throw e;
			} catch (Exception e) {
				throw new RecordException("Error calculation Occurs Depending On for Variable: " + dependingOnDef.getVariableName() + " msg="+ e.getMessage(), e); 
			}
		} 
		return tmpAdj;	
	}

	@Override
	public void checkForSizeFieldUpdate(AbstractLine line, IFieldDetail fld) {
		
	}

	@Override
	public void clearBuffers(AbstractLine line) {
		
	}
}
