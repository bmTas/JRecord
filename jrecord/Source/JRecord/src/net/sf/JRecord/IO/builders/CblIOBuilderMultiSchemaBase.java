package net.sf.JRecord.IO.builders;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.ArrayList;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICopybookLoaderStream;
import net.sf.JRecord.External.ISetDropCopybookName;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ICopybookDialects;


public class CblIOBuilderMultiSchemaBase<IOB> extends CblIOBuilderBase<IOB> implements IGetLoader  { 

	private final String copybookname;
	final ICopybookLoaderStream loader;
	final ArrayList<ICreateExternal> copybooks = new ArrayList<ICreateExternal>();


	/**
	 * Create Multicopybokk builder
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 */
	public CblIOBuilderMultiSchemaBase(String copybookname, ICopybookLoaderStream loader) {
		this(copybookname, loader, ICopybookDialects.FMT_MAINFRAME);
	}
	
	/**
	 * Create Multicopybokk builder
	 * @param copybookName name of the copybook
	 * @param loader copybook (schema) loader
	 */
	protected CblIOBuilderMultiSchemaBase(String copybookname, ICopybookLoaderStream loader, int dialect) {
		super(dialect);
		this.copybookname = copybookname;
		this.loader = loader;
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder#addCopyBook(java.lang.String)
	 */
	public IOB addCopyBook(String fileName) {
		copybooks.add(new CreateExternalFromFile(this, fileName));
		return super.self;
	} 

	public IOB addCopyBook(InputStream inStream, String copybookName) {
		copybooks.add(new CreateExternalFromStream(this, inStream, copybookName));
		return super.self;
	}

	public IOB addCopyBook(Reader reader, String copybookName) {
		copybooks.add(new CreateExternalFromReader(this, reader, copybookName));
		return super.self;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#getExternalRecordImpl()
	 */
	@Override
	protected ExternalRecord getExternalRecordImpl() throws IOException { 
		
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
				ExternalRecord rec = ExternalRecord.getNullRecord(copybookname, Constants.rtGroupOfRecords, super.getFont());
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
	public IOB setSplitCopybook(int splitCopybook) {
		if (copybooks.size() == 0) {
			throw new RuntimeException("You can only use setSplitCopybook after you have added a copybook !!!");
		}
		copybooks.get(copybooks.size() - 1).setSplitCopybook(splitCopybook);
		super.setSplitCopybook(splitCopybook);
		clearLayout();
		
		return super.self;
	}
	
	public IOB setRecordSelectionCurrentCopybook(ExternalSelection recordSelection) {
		if (copybooks.size() == 0) {
			throw new RuntimeException("You can only use setRecordSelectionCurrentCopybook after you have added a copybook !!!");
		}
		copybooks.get(copybooks.size() - 1).setRecordSelection(recordSelection);
		clearLayout();
		
		return super.self; 

	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.IGetLoader#getLoader()
	 */
	@Override
	public final ICopybookLoaderStream getLoader() {
		return loader;
	}

	
	/**
	 * @return the dialect
	 */
	@Override
	public final int getDialect() {
		return dialect;
	}


	/**
	 * @return the copybookFileFormat
	 */
	@Override
	public final int getCopybookFileFormat() {
		return copybookFileFormat;
	}


	/**
	 * @return the log
	 */
	@Override
	public final AbsSSLogger getLog() {
		return log;
	}
}
