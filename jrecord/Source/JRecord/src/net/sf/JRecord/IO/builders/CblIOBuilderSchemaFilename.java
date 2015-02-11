package net.sf.JRecord.IO.builders;

import java.io.IOException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ISetDropCopybookName;

public class CblIOBuilderSchemaFilename extends CblIOBuilderBase {

	private final String copybookFilename;
	private final CopybookLoader loader;

	public CblIOBuilderSchemaFilename(String copybookFilename, CopybookLoader loader, int cobolDialect) {
		super(cobolDialect);
		this.copybookFilename = copybookFilename;
		this.loader = loader;
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#getExternalRecordImpl()
	 */
	@Override
	protected ExternalRecord getExternalRecordImpl() throws RecordException,
			IOException {
		
		if (loader instanceof ISetDropCopybookName) { 
			((ISetDropCopybookName) loader).setDropCopybookFromFieldNames(super.dropCopybookNameFromFields);
		}
		try {

			return loader	.loadCopyBook(copybookFilename, splitCopybook, 0, font, super.copybookFileFormat, dialect, 0, log);

		} catch (RecordException e) {
			throw e; 
		} catch (IOException e) {
			throw e;
		} catch (Exception e) {
			throw new RecordException(e.getMessage(), e);
		} 
	}
}
