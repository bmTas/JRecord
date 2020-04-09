/**
 * 
 */
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
 * This is a placeholder implementation for very complicated OD
 * <pre>
 * 
 *         05 Size-1                     pic s9(4) comp.
 *         05 occurs 1 to 100 times depending on Size-1.
 *            10 Size-2                  pic s9(4) comp.
 *            10 occurs 1 to 100 times depending on Size-2.
 * 
 * </pre>
 * The problem
 * @author Bruce Martin
 *
 */
public class ODCalculationComplex implements IOccursDependingPositionCalculation {

	private static final ODLastLookupDetails emptyLastLookup = new ODLastLookupDetails(null);
//	private final ODCalculationStandard stdCalc;
	private final DependingOnDefinition dependingOn;
	
	private ODLastLookupDetails lastDetails = emptyLastLookup;
	
	public static long optCount = 0;
	public static long normalCount = 0;
	
	public ODCalculationComplex(DependingOnDefinition dependingOn) {
		super();
//		this.stdCalc = new ODCalculationStandard(dependingOn);
		this.dependingOn = dependingOn;
		
//		System.out.println("");
//		System.out.println("*------------------------------------------------------------------*");
//		System.out.println("  This Cobol Copybook uses Complicated Occurs Depending");
//		System.out.println("i.e. OD Size is an array field. Sample of complex Occurs Depending");
//		System.out.println("");
//		System.out.println("       05 Size-1                     pic s9(4) comp.       ");
//		System.out.println("       05 occurs 1 to 100 times depending on Size-1.");
//		System.out.println("          10 Size-2                  pic s9(4) comp.");
//		System.out.println("          10 occurs 1 to 100 times depending on Size-2.");
//		System.out.println("");
//		System.out.println("  This has been added recently so expect problems");
//		System.out.println("*------------------------------------------------------------------*");
//		System.out.println("");
	}

	
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
		ODLastLookupDetails lastDtls = lastDetails;
		if (lastDtls.line != line) {
			lastDtls = new ODLastLookupDetails(line, dependingOn); 
		}

		if (dependingOnDtls != null ) {
			if (pos >= dependingOnDtls.getReliableCalculationsTo()) {
				
			} else if (lastDtls.line == line
					&& lastDtls.savedDependOnDtls.dependOnDtls == dependingOnDtls) {
				optCount += 1;
				return lastDtls.savedDependOnDtls.adjustment;
			} else {
				tree = dependingOnDtls.getTree();
				
				int adj = calculateAdjustment(lastDtls, dependingOnList, tree, 0, pos);
				lastDtls.setDependingOnDtls(dependingOnDtls, adj);
				lastDetails = lastDtls;
				
				normalCount += 1;
				return adj;
			}
			
			tree = dependingOnDtls.getTree();			
		}
		
		normalCount += 1;
		lastDetails = lastDtls;
		return calculateAdjustment(lastDtls, dependingOnList, tree, 0, pos);
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
	private int calculateAdjustment(
			ODLastLookupDetails lastDtls,
			List<DependingOn> dependingOnList, //final AbstractIndexedLine line, 
			DependingOnDtls[] dependingOnDtls, int lvl,
			final int pos) {
		if (dependingOnList == null || dependingOnList.size() == 0 || pos < dependingOnList.get(0).getPosition()) {
			return 0;
		}

		int tmpAdj = 0;
		
		for (int i = 0; i < dependingOnList.size() && pos >= dependingOnList.get(i).getPosition(); i++) {
			DependingOn dependingOnDef = dependingOnList.get(i);
			IFieldDetail field = dependingOnDef.getField();	

			int adj = 0;
			try {
//				Object value = line.getField(field);
				List<DependingOn> children = dependingOnDef.getIndexDtls().get(0).getChildren();
				
//				String trim = value == null? "" : value.toString().trim();
//				if (trim.length() > 0) {
					int actualOccurs = lastDtls.getFieldValue(dependingOnDef);//Integer.parseInt(trim);
					int occursLength = dependingOnDef.getOccursLength();
					boolean stdCalc = false;
	
					//int childAdjustment = calculateAdjustment(children, line, dependingOnDtls, lvl+1, pos);
					if (pos > dependingOnDef.getEnd()) {
						int occursMaxLength = dependingOnDef.getOccursMaxLength();
						int actualLength = 0;
						
						
						if (stdCalc) {
							int actualOccursLength = occursLength 
											 - calculateAdjustment(lastDtls, children, dependingOnDtls, lvl+1, pos); 
							actualLength = actualOccurs * actualOccursLength;
						} else {
							for (int j = 0; j < actualOccurs; j++) {
								actualLength += occursLength 
											  - calculateAdjustment(
													  lastDtls, 
													  dependingOnDef.getIndexDtls().get(j).getChildren(), 
													  dependingOnDtls, lvl+1,
													  pos); 
							}
						}
		//						- calculateAdjustment(dependingOnDef.getChildren(), line, pos);
						adj = occursMaxLength - actualLength;//calculateAdjustment(dependingOnDef.getChildren(), line, pos);
						if (pos - adj < dependingOnDef.getPosition() + actualLength) {
							return tmpAdj;
						}
					} else if (children != null && children.size() > 0) { 
						int idx = dependingOnDtls == null ? 0 : dependingOnDtls[lvl].index;
						if (stdCalc) {
							int childAdjustment = calculateAdjustment(lastDtls, children, dependingOnDtls, lvl+1, pos);
							if (dependingOnDtls != null && childAdjustment > 0) {
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
										int tChildAdj = calculateAdjustment(
												lastDtls, 
												children,
												dependingOnDtls, 
												lvl+1,
												pos - occurs * occursLength);	
										adj += tChildAdj;						
									}
								}
							} else {
								adj = childAdjustment;
							}
						} else {
							int end = Math.min(idx+1, dependingOnDef.getOccursMax());
							for (int j = 0; j < end; j++) {
								List<DependingOn> children2 = dependingOnDef.getIndexDtls().get(j).getChildren();
								int a = calculateAdjustment(
										  lastDtls, 
										  children2,
										  dependingOnDtls, lvl+1,
										  pos);
								adj += a;
								DependingOn c = children2.get(children.size() - 1);
								if (pos < c.getEnd()) {
									break;
								}
							}
						}
					}
					tmpAdj += adj;
//				}
			} catch (RuntimeException e) {
				System.out.println();
				System.out.println("ODCalculationComplex ~ Error Retrieving: " + (field==null?"null field":field.getName()));
				System.out.println(e);

				throw e;
			} catch (Exception e) {
				throw new RecordException("Error calculation Occurs Depending On for Variable: " + dependingOnDef.getVariableName() + " msg="+ e.getMessage(), e); 
			}
		} 
		return tmpAdj;	
	}
	
	/**
	 * Check if a Occurs Depending Size field has been updated
	 * @param line line being updated
	 * @param fld field being updated
	 * @param value the new value
	 */
	@Override
	public void checkForSizeFieldUpdate(AbstractLine line, IFieldDetail fld) {
		ODLastLookupDetails lastDtls = lastDetails;
		DependingOnDefinition.SizeField sizeFld;
		if (lastDtls.line == line && fld != null && ( (sizeFld = dependingOn.getSizeField(fld.getPos())) != null) ) {
			lastDtls.clearSizeFieldNumber(sizeFld.fieldNumber);
		}
	}
	
	
	/**
	 * Clear the 'Array Size' buffers
	 */
	@Override
	public void clearBuffers(AbstractLine line) {
		if (lastDetails.line == line) {
			lastDetails = emptyLastLookup;
		}
	}
	
//	private static class Adjustment {
//		boolean moreToDo = true; 
//		int adj = 0;
//	}

}
