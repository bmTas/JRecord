package net.sf.JRecord.IO.builders;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.External.ISetDropCopybookName;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class CblIOBuilderSchemaStream extends CblIOBuilderBase implements ICobolIOBuilder {

	private final InputStream copybookStream;
	private final String copybookName;
	private final ICopybookLoaderStream loader;

	
	/**
	 * Create a IOBuilder for a Stream based Copybook (schema)
	 * 
	 * @param copybookStream copybook
	 * @param copybookName
	 * @param copybookType
	 * @param cobolDialect
	 */
	public CblIOBuilderSchemaStream(InputStream copybookStream, String copybookName, ICopybookLoaderStream loader, int cobolDialect) {
		super(cobolDialect);
		this.copybookName = copybookName;
		this.copybookStream = copybookStream;
		this.loader = loader;
	} 

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#getExternalRecordImpl()
	 */
	@Override
	protected ExternalRecord getExternalRecordImpl() throws RecordException, IOException {
		if (loader instanceof ISetDropCopybookName) {
			((ISetDropCopybookName) loader).setDropCopybookFromFieldNames(super.dropCopybookNameFromFields);
		}
						
		return  loader	.loadCopyBook(copybookStream, copybookName, splitCopybook, 0, font, super.copybookFileFormat, dialect, 0, log);
	}

}
