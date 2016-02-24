package net.sf.JRecord.IO.builders;

import net.sf.JRecord.External.ExternalRecord;

public class CreateExternalFromFile extends CreateExternalBase implements ICreateExternal  {

	private final String copybookFilename;
//	private final CblIOBuilderMultiSchema parent;

	

	public CreateExternalFromFile(IGetLoader parent, String copybookFilename) {
		super(parent); 
		this.copybookFilename = copybookFilename;
	} 


	@Override
	public ExternalRecord createExternalRecordImp() throws Exception {
		return parent.getLoader().loadCopyBook(copybookFilename, splitCopybook, 0, parent.getFont(), 
				parent.getCopybookFileFormat(), parent.getDialect(), 0, parent.getLog());
	}
}
