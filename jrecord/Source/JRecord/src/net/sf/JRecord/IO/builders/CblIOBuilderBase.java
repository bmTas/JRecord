package net.sf.JRecord.IO.builders;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;

/**
 * Base class for the various IOBuilders. IOBuilders are used to create
 * LineReaders (readers for Cobol files) and LineWriter (Writers for cobol files)
 * 
 * @author Bruce Martin
 *
 */
public abstract class CblIOBuilderBase implements ICobolIOBuilder {

	private static final TextLog DEFAULT_LOG = new TextLog();

	private LayoutDetail layout = null;
	private LineProvider lineProvider;

	int dialect;
    //final int copybookType;

	int splitCopybook = CopybookLoader.SPLIT_NONE;
	String font = "";
	int copybookFileFormat = 1; // actually Cb2xmlConstants.USE_STANDARD_COLUMNS;
	private int fileOrganization = Constants.NULL_INTEGER;
	boolean dropCopybookNameFromFields = false;
	
    AbsSSLogger log = DEFAULT_LOG; 
    
	protected CblIOBuilderBase(int dialect) {
		super();
		//this.copybookType = copybookType;
		this.dialect = dialect;
	}




	/**
	 * @param dialect the dialect to set
	 */
	public final ICobolIOBuilder setDialect(int dialect) {
		this.dialect = dialect;
		clearLayout();
		return this;
	}




	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#setSplitCopybook(int)
	 */
	@Override
	public final ICobolIOBuilder setSplitCopybook(int splitCopybook) {
		this.splitCopybook = splitCopybook;
		clearLayout();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#setFont(java.lang.String)
	 */
	@Override
	public final ICobolIOBuilder setFont(String font) {
		this.font = font;
		clearLayout();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#setCopybookFileFormat(int)
	 */
	@Override
	public final ICobolIOBuilder setCopybookFileFormat(int copybookFileFormat) {
		this.copybookFileFormat = copybookFileFormat;
		clearLayout();
		return this;
	}

	/**
	 * @param fileOrganization the fileOrganization to set
	 */
	@Override
	public final ICobolIOBuilder setFileOrganization(int fileOrganization) {
		this.fileOrganization = fileOrganization;
		clearLayout();
		return this;
	}




	/**
	 * @param dropCopybookNameFromFields the dropCopybookNameFromFields to set
	 */
	@Override
	public final ICobolIOBuilder setDropCopybookNameFromFields(
			boolean dropCopybookNameFromFields) {
		this.dropCopybookNameFromFields = dropCopybookNameFromFields;
		clearLayout();
		return this;
	}




	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#setLog(net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public final ICobolIOBuilder setLog(AbsSSLogger log) {
		this.log = log;
		clearLayout();
		return this;
	} 
	
	
    /**
	 * @see net.sf.JRecord.IO.builders.ICobolIOBuilder#newLine()
	 */
	@Override
	public AbstractLine newLine() throws IOException, RecordException {
		LayoutDetail schema = getLayout();
		
		return lineProvider.getLine(schema);
	}




	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newReader(java.lang.String)
	 */
	@Override
	public final AbstractLineReader newReader(String filename) throws FileNotFoundException, IOException, RecordException {
		return newReader(new FileInputStream(filename));
	}
	
    /* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newReader(java.io.InputStream)
	 */
	@Override
	public final AbstractLineReader newReader(InputStream datastream) throws IOException, RecordException {
		LayoutDetail schema = getLayout();
		AbstractLineReader r = LineIOProvider.getInstance().getLineReader(schema);
		
		r.open(datastream, schema);
		return r;
	}
	
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newWriter(java.lang.String)
	 */
	@Override
	public final AbstractLineWriter newWriter(String filename) throws FileNotFoundException, IOException, RecordException {
		return newWriter(new FileOutputStream(filename));
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newWriter(java.io.OutputStream)
	 */
	@Override
	public final AbstractLineWriter newWriter(OutputStream datastream) throws IOException, RecordException {
		LayoutDetail schema = getLayout();
		AbstractLineWriter r = LineIOProvider.getInstance().getLineWriter(schema);
		
		r.open(datastream);
		return r;
	}
	
	/**
	 * Used to test if attributes are set correctly
	 * @return all the attributes
	 */
	protected final Object[] getAllAttributes() {
		Object[] r = {
				dialect,
				splitCopybook,
				copybookFileFormat,// actually Cb2xmlConstants.USE_STANDARD_COLUMNS;
				fileOrganization,
				font,
				dropCopybookNameFromFields,
		};
		 
		return r;

	}
	
	
	@Override
	public final ExternalRecord getExternalRecord() throws RecordException, IOException  {			
		ExternalRecord schema =  getExternalRecordImpl();
		
		if (fileOrganization >= 0) {
			schema.setFileStructure(fileOrganization);
		}
		return schema; 
	}
	
	protected abstract ExternalRecord getExternalRecordImpl() throws RecordException, IOException;

	protected final void clearLayout() {
		layout = null;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#getLayout()
	 */
	@Override
	public final LayoutDetail getLayout() throws RecordException, IOException {
		if (layout == null) {
			layout = getExternalRecord()	.asLayoutDetail();
			lineProvider = LineIOProvider.getInstance().getLineProvider(layout);
		}
		return layout;
	}


}
