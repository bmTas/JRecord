package net.sf.JRecord.IO.builders;

import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;

/**
 * For internal use in JRecord, it combines the various IOBuilders so that 
 * one Base class can be used !!!
 * 
 * @author Bruce Martin
 *
 */
interface IIOBldrAll extends ICsvIOBuilder, IFixedWidthIOBuilder, ICobolMultiCopybookIOBuilder {


	public abstract IIOBldrAll setFileOrganization(int fileOrganization);
	
	public abstract IIOBldrAll setFont(String font);

	public abstract IIOBldrAll setRecordSelection(String recordName, ExternalSelection selectionCriteria);
}
