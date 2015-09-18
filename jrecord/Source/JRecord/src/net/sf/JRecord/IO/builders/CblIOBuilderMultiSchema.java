package net.sf.JRecord.IO.builders;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.External.ISetDropCopybookName;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;

public class CblIOBuilderMultiSchema extends CblIOBuilderBase implements ICobolMultiCopybookIOBuilder {

	private final String copybookname;
	final ICopybookLoaderStream loader;
	final ArrayList<ICreateExternal> copybooks = new ArrayList<ICreateExternal>();

	/**
	 * Create a IOBuilder for a file based Copybook (schema)
	 * @param copybookFilename copybook file name
	 * @param loader copybook (schema) loader
	 * @param cobolDialect
	 */
	public CblIOBuilderMultiSchema(String copybookFilename, ICopybookLoaderStream loader, int cobolDialect) {
		super(cobolDialect);
		this.loader = loader;
		this.copybookname = copybookFilename;
		copybooks.add(new CreateExternalFromFile(this, copybookFilename));
	}


	/**
	 * Create a IOBuilder for a Stream based Copybook (schema)
	 * 
	 * @param copybookStream copybook stream
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 * @param cobolDialect Cobol Dialect
	 * @throws IOException Any IO Error that occured
	 */
	public CblIOBuilderMultiSchema(InputStream copybookStream, String copybookName, ICopybookLoaderStream loader, int cobolDialect) throws IOException {
		super(cobolDialect);

		this.loader = loader;
		this.copybookname = copybookName;
		
		copybooks.add(new CreateExternalFromStream(this, copybookStream, copybookName));
	} 

	/**
	 * Create Multicopybokk builder
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 */
	public CblIOBuilderMultiSchema(String copybookname, ICopybookLoaderStream loader) {
		super(ICopybookDialects.FMT_MAINFRAME);
		this.copybookname = copybookname;
		this.loader = loader;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder#addCopyBook(java.lang.String)
	 */
	@Override
	public ICobolMultiCopybookIOBuilder addCopyBook(String fileName) {
		copybooks.add(new CreateExternalFromFile(this, fileName));
		return this;
	}

	@Override
	public ICobolMultiCopybookIOBuilder addCopyBook(InputStream inStream, String copybookName) {
		copybooks.add(new CreateExternalFromStream(this, inStream, copybookName));
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#getExternalRecordImpl()
	 */
	@Override
	protected ExternalRecord getExternalRecordImpl() throws RecordException,
			IOException { 
		
		if (copybooks.size() == 0) {
			throw new RecordException("No copybooks have been specified");
		}
		if (loader instanceof ISetDropCopybookName) { 
			((ISetDropCopybookName) loader).setDropCopybookFromFieldNames(super.dropCopybookNameFromFields);
		}
		try {
			 
			if (copybooks.size() == 1) {
				return copybooks.get(0).createExternalRecord();
			} else {
				ExternalRecord rec = ExternalRecord.getNullRecord(copybookname, Constants.rtGroupOfRecords, super.font);
				ExternalRecord r;
				
				for (ICreateExternal copybookdef : copybooks) {
					r = copybookdef.createExternalRecord();
					if (r.getNumberOfRecords() == 0) {
						rec.addRecord(r);
					} else {
						for (int i = 0; i < r.getNumberOfRecords(); i++) {
							rec.addRecord(r.getRecord(i));
						}
					}
				}
				
				return rec;
			}

		} catch (RecordException e) {
			throw e; 
		} catch (IOException e) {
			throw e;
		} catch (Exception e) {
			throw new RecordException(e.getMessage(), e);
		} 
	}
	

	/**
	 * @see net.sf.JRecord.def.IO.ICobolIOBuilder#setSplitCopybook(int)
	 */
	@Override
	public ICobolMultiCopybookIOBuilder setSplitCopybook(int splitCopybook) {
		if (copybooks.size() == 0) {
			throw new RuntimeException("You can only use setSplitCopybook after you have added a copybook !!!");
		}
		copybooks.get(copybooks.size() - 1).setSplitCopybook(splitCopybook);
		clearLayout();
		
		return this;
	}
	
	@Override
	public ICobolMultiCopybookIOBuilder setRecordSelectionCurrentCopybook(ExternalSelection recordSelection) {
		if (copybooks.size() == 0) {
			throw new RuntimeException("You can only use setRecordSelectionCurrentCopybook after you have added a copybook !!!");
		}
		copybooks.get(copybooks.size() - 1).setRecordSelection(recordSelection);
		clearLayout();
		
		return this;

	}

}
