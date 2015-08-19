package net.sf.JRecord.IO.builders;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.TreeMap;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLineProvider;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IDefineCsvFields;
import net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByLength;
import net.sf.JRecord.def.IO.builders.IDefineFixedFieldsByPosition;

/**
 * Base class for the various IOBuilders. IOBuilders are used to create
 * LineReaders (readers for Cobol files) and LineWriter (Writers for cobol files)
 * 
 * @author Bruce Martin
 *
 */
public abstract class CblIOBuilderBase implements IIOBldrAll {

	private static final TextLog DEFAULT_LOG = new TextLog();

	private LayoutDetail layout = null;
	private LineProvider lineProvider;
	Map<String, ExternalSelection> recordSelectionMap = null;

	int dialect;
    //final int copybookType;

	int splitCopybook = CopybookLoader.SPLIT_NONE;
	String font = "";
	int copybookFileFormat = 1; // actually Cb2xmlConstants.USE_STANDARD_COLUMNS;
	int fileOrganization = Constants.NULL_INTEGER;
	boolean dropCopybookNameFromFields = false;
	
    AbsSSLogger log = DEFAULT_LOG; 
    
	protected CblIOBuilderBase(int dialect) {
		super();
		//this.copybookType = copybookType;
		this.dialect = dialect;
	}


	protected CblIOBuilderBase(LayoutDetail schema) {
		super();
		//this.copybookType = copybookType;
		this.layout = schema;
	}


	/**
	 * @param dialect the dialect to set
	 */
	public final ICobolIOBuilder setDialect(int dialect) {
		this.dialect = dialect;
		clearLayout();
		return this;
	}




	/**
	 * @see net.sf.JRecord.def.IO.ICobolIOBuilder#setSplitCopybook(int)
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
	public final IIOBldrAll setFont(String font) {
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
	 * @return the fileOrganization
	 */
	protected int getFileOrganization() {
		return fileOrganization;
	}

	/**
	 * @param fileOrganization the fileOrganization to set
	 */
	@Override
	public IIOBldrAll setFileOrganization(int fileOrganization) {
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
	public final IIOBldrAll setLog(AbsSSLogger log) {
		this.log = log;
		clearLayout();
		return this;
	} 
	
	
    /**
	 * @see net.sf.JRecord.def.IO.builders.ICobolIOBuilder#newLine()
	 */
	@Override
	public AbstractLine newLine() throws IOException, RecordException {
		LayoutDetail schema = getLayout();
		
		return lineProvider.getLine(schema);
	}


    /**
	 * @see net.sf.JRecord.def.IO.builders.ICobolIOBuilder#newLine()
	 */
	@Override
	public AbstractLine newLine(byte[] data) throws IOException, RecordException {
		LayoutDetail schema = getLayout();
		
		return lineProvider.getLine(schema, data);
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newReader(java.lang.String)
	 */
	@Override
	public final AbstractLineReader newReader(String filename) throws FileNotFoundException, IOException, RecordException {
		//checkOk(true);
		return newReader(new FileInputStream(filename));
	}
	
    /* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newReader(java.io.InputStream)
	 */
	@Override
	public final AbstractLineReader newReader(InputStream datastream) throws IOException, RecordException {
		checkOk(true);
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
		checkOk(false);
		return newWriter(new FileOutputStream(filename));
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.IIOBuilder#newWriter(java.io.OutputStream)
	 */
	@Override
	public final AbstractLineWriter newWriter(OutputStream datastream) throws IOException, RecordException {
		checkOk(false);
		LayoutDetail schema = getLayout();
		AbstractLineWriter r = LineIOProvider.getInstance().getLineWriter(schema);
		
		r.open(datastream);
		return r;
	}
	
	
	/**
	 * Method to allow ChildBuilders to validate the schema prior to
	 * creating Reader / Writer
	 * @param input
	 * @throws RecordException
	 */
	protected void checkOk(boolean input) throws RecordException {
		
	}
	
	/**
	 * Get all the attributes:
	 * <pre>
	 *     dialect,
	 *     splitCopybook,
	 *     copybookFileFormat,
	 *     fileOrganization,
	 *     font,
	 *     dropCopybookNameFromFields,
	 * </pre>    
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
	
	
	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.ICobolIOBuilder#setRecordSelection(java.lang.String, net.sf.JRecord.ExternalRecordSelection.ExternalSelection)
	 */
	@Override
	public IIOBldrAll setRecordSelection(String recordName, ExternalSelection selectionCriteria) {
		if (recordSelectionMap == null) {
			recordSelectionMap = new TreeMap<String, ExternalSelection>();
		}
		
		recordSelectionMap.put(recordName.toLowerCase(), selectionCriteria);
		return this;
	}




	@Override
	public final ExternalRecord getExternalRecord() throws RecordException, IOException  {			
		ExternalRecord schema =  getExternalRecordImpl();
		
		if (fileOrganization >= 0) {
			schema.setFileStructure(fileOrganization);
		}
		
		if (recordSelectionMap != null) {
			for (int i = 0; i < schema.getNumberOfRecords(); i++) {
				ExternalRecord record = schema.getRecord(i);
				ExternalSelection selection = recordSelectionMap.get(record.getRecordName().toLowerCase());
				if (selection != null) {
					record.setRecordSelection(selection);
				}
			}
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
		
		if (layout.hasBinaryField()) {
			if (CommonBits.getLineType(layout.getFileStructure()) != CommonBits.LT_BYTE) {
				throw new RuntimeException("This is a binary Layout (Schema) but you have selected a Character file Organisation,"
						+ " you must use you must Byte based Organisation");
			}
			if (lineProvider instanceof CharLineProvider) {
				throw new RuntimeException("This is a binary schema (requires byte based line-provider), but the line provider is a Text Based Line provider");
			}
		}

		return layout;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder#defineFieldsByPosition()
	 */
	@Override
	public IDefineFixedFieldsByPosition defineFieldsByPosition() {
		throw new RuntimeException("JRecord Error - this method should not be called in CblIOBuilderBase");
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder#defineFieldsByLength()
	 */
	@Override
	public IDefineFixedFieldsByLength defineFieldsByLength() {
		throw new RuntimeException("JRecord Error - this method should not be called in CblIOBuilderBase");
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.ICsvIOBuilder#defineFields()
	 */
	@Override
	public IDefineCsvFields defineFields() {
		throw new RuntimeException("JRecord Error - this method should not be called in CblIOBuilderBase");
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.ICsvIOBuilder#setDelimiter(java.lang.String)
	 */
	@Override
	public ICsvIOBuilder setDelimiter(String val) {
		return null;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.ICsvIOBuilder#setQuote(java.lang.String)
	 */
	@Override
	public ICsvIOBuilder setQuote(String val) {
		return null;
	}
	
	
}
