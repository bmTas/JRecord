package net.sf.JRecord.IO.builders;

import java.io.IOException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;

public interface ICreateExternal {

	/**
	 * Create the requested External Record
	 * @param splitCopybook wether to split the copybook
	 * @return requested External Record
	 * 
	 * @throws RecordException
	 * @throws IOException
	 */
	public abstract ExternalRecord createExternalRecord()
			throws Exception;

	public abstract void setSplitCopybook(int splitCopybook);
	
	public abstract void setRecordSelection(ExternalSelection recordSelection);

}