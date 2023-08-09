package net.sf.JRecord.cbl2csv.imp;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.cbl2csv.args.IUpdateFieldName;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

/**
 * 
 * @author Bruce Martin
 *
 */
public class CobolToCsvBldr implements ICobolToCsvDefinition, ICobolToCsvBldr {
    private LayoutDetail schema=null;
    private String separator="\t", quote="\"",  outFileName, recordVariable, outputCharacterSet, 
    		lowValueTxt="Low-Values", highValueTxt="High-Values", numericSpacesTxt="";
    private AbstractLineReader lineReader;
    private BufferedWriter csvWriter;
    
    private boolean writeRecordName = false, csvHeader=true, reportInvalidFields=false;
    private IUpdateFieldName updateFieldName=new IUpdateFieldName() {		
		@Override public String updateName(String name) {
			return name;
		}
	};
    
	/**
	 * Create a new Cobol IOBulder from a COBOL-Copybook file
	 * 
	 *<pre>
	 *<b>Example:</b>
	 * 
	 *      AbstractLineReader r = CobolToCsvBldr
	 *              .<b>newIOBuilder("file-name")</b>
	 *                  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
	 *                  .setDialect(ICopybookDialects.FMT_FUJITSU)
	 *              .newReader("Data-Filename");
	 * </pre> 
	 * 
	 * @param copybookFileame name of the COBOL-Copybook stream.
	 * 
	 * These are the default values (which can be overriden with the appropriate set* method
	 * @return requested IOBuilder
	 * 
	 *<pre> </pre>
	 *
	 * <b>Main Methods:</b><ul>
	 *  <li><b>setFileOrganization</b> Set the <i>file organization</i> (or Structure). While a Windows/Unix style
	 * file organization is the most common (supported by Constants.<b>IO_STANDARD_TEXT_FILE</b> or Constants.<b>IO_UNICODE_TEXT</b> in JRecord).
	 * There others including (but is not limited to):
	 * <pre>
	 *     <b>Variable Length</b> where the length is before the Record Data:
	 *     
	 *           &lt;Record-LengthFixed-Sized-record-Data&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;&lt;Record-Length&gt;&lt;record-Data&gt;
	 *           
	 *     <b>Fixed-Length</b> where all records a of a constant fixed Length:
	 *     
	 *          &lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;&lt;Fixed-Sized-record-Data&gt;
	 *          
	 *     <b>CSV files</b> with \n embedded in Quotes is another variation
	 * </pre>
	 *  <li><b>setSplitCopybook</b> Wether the Cobol-Copybook should be split into sub-record's or not and how it should be split.
	 *  <li><b>setDialect</b Set the Cobol dialect (is it Mainframe, GNU-Cobol etc).
	 *  <li><b>setCopybookFileFormat</b> - is a standard Column 6-72 or some other format
	 *  <li><b>setFont</b> Set the font (character-set) used in the Data-File.
	 *  </ul>
	 *  
	 *     
     * The CodeGen utility can generate basic JRecord code. It is available<ul>
     * <li>as part of the <b>RecordEditor</b> see 
     * <a href="http://record-editor.sourceforge.net/RecordEditorGenerate.htm#HDRJRECGEN">http://record-editor.sourceforge.net/RecordEditorGenerate.htm#HDRJRECGEN</a>
     * <li>As a standalone download: <a href="https://sourceforge.net/projects/jrecord/files/jrecord_CodeGen/">https://sourceforge.net/projects/jrecord/files/jrecord_CodeGen/</a>
     * </ul>
	 * 
	 */
    public static ICobolIOBuilder newCobolIOBuilder(String copybookFileame) {
    	return JRecordInterface1.COBOL.newIOBuilder(copybookFileame);
    }
    
    
	/**
	 * Create IO Builder with reader
	 * @param copybookReader Cobol Copybook Reader
	 * @param copybookName Name of the Cobol Copybook
	 * @return Cobol IOBuilder
	 * 
	 *     * 
     * The CodeGen utility can generate basic JRecord code. It is available<ul>
     * <li>as part of the <b>RecordEditor</b> see 
     * <a href="http://record-editor.sourceforge.net/RecordEditorGenerate.htm#HDRJRECGEN">http://record-editor.sourceforge.net/RecordEditorGenerate.htm#HDRJRECGEN</a>
     * <li>As a standalone download: <a href="https://sourceforge.net/projects/jrecord/files/jrecord_CodeGen/">https://sourceforge.net/projects/jrecord/files/jrecord_CodeGen/</a>
     * </ul>
	 */
    public static ICobolIOBuilder newCobolIOBuilder(Reader copybookReader, String copybookName) {
    	return JRecordInterface1.COBOL.newIOBuilder(copybookReader, copybookName);
    }
    
    
    public static ICobolToCsvBldr newMultiRecordCsvBuilder() {
    	return new CobolToCsvBldr();
    }
    
    private List<RecordWriterDetails> recordList = new ArrayList<RecordWriterDetails>();

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvDefinition#getSchema()
	 */
	@Override
	public final LayoutDetail getSchema() {
		if (schema == null && lineReader != null) {
			schema = lineReader.getLayout();
		}
		return schema;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr#setSchema(net.sf.JRecord.Details.LayoutDetail)
	 */
	@Override
	public final CobolToCsvBldr setSchema(LayoutDetail schema) {
		this.schema = schema;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvDefinition#getSeperator()
	 */
	@Override
	public final String getSeparator() {
		return separator;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr#setSeperator(java.lang.String)
	 */
	@Override
	public final CobolToCsvBldr setSeparator(String separator) {
		this.separator = separator;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvDefinition#getQuote()
	 */
	@Override
	public final String getQuote() {
		return quote;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr#setQuote(java.lang.String)
	 */
	@Override
	public final CobolToCsvBldr setQuote(String quote) {
		this.quote = quote;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvDefinition#isWriteRecordName()
	 */
	@Override
	public final boolean isWriteRecordName() {
		return writeRecordName;
	}

	/**
	 * @return the csvHeader
	 */
	@Override
	public final boolean isCsvHeader() {
		return csvHeader;
	}


	/**
	 * @param csvHeader the csvHeader to set
	 */
	@Override
	public final CobolToCsvBldr setCsvHeader(boolean csvHeader) {
		this.csvHeader = csvHeader;
		return this;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvBldr#setWriteRecordName(boolean)
	 */
	@Override
	public final CobolToCsvBldr setWriteRecordName(boolean writeRecordName) {
		this.writeRecordName = writeRecordName;
		return this;
	}

	/**
	 * @return the lowValueTxt
	 */
	@Override
	public final String getLowValueTxt() {
		return lowValueTxt;
	}


	/**
	 * @param lowValueTxt the lowValueTxt to set
	 */
	@Override
	public final CobolToCsvBldr setLowValueTxt(String lowValueTxt) {
		this.lowValueTxt = lowValueTxt;
		this.reportInvalidFields = true;
		return this;
	}


	/**
	 * @return the highValueTxt
	 */
	@Override
	public final String getHighValueTxt() {
		return highValueTxt;
	}


	/**
	 * @param highValueTxt the highValueTxt to set
	 */
	@Override
	public final CobolToCsvBldr setHighValueTxt(String highValueTxt) {
		this.highValueTxt = highValueTxt;
		this.reportInvalidFields = true;
		return this;
	}


	/**
	 * @return the numericSpaces
	 */
	@Override
	public final String getNumericSpacesTxt() {
		return numericSpacesTxt;
	}


	/**
	 * @param numericSpacesText Text to use when a numeric field is spaces
	 */
	@Override
	public final CobolToCsvBldr setNumericSpacesTxt(String numericSpacesText) {
		this.numericSpacesTxt = numericSpacesText;
		this.reportInvalidFields = true;
		return this;
	}


	/**
	 * @return the reportInvalidFields
	 */
	@Override
	public final boolean isReportInvalidFields() {
		return reportInvalidFields;
	}


	/**
	 * @param reportInvalidFields the reportInvalidFields to set
	 */
	@Override
	public final CobolToCsvBldr setReportInvalidFields(boolean reportInvalidFields) {
		this.reportInvalidFields = reportInvalidFields;
		return this;
	}


	/* (non-Javadoc)
	 * @see net.sf.JRecord.cbl2csv.imp.ICobolToCsvDefinition#getRecordList()
	 */
	@Override
	public final List<RecordWriterDetails> getRecordList() throws IOException {
		LayoutDetail fileSchema = getSchema();
		if (recordList.size() == 0 && fileSchema != null&& recordVariable != null && recordVariable.length() > 0 
		&& outFileName != null) {
			for (int i = 0; i < fileSchema.getRecordCount(); i++) {
				String recordName = fileSchema.getRecord(i).getRecordName();
				recordList.add(new RecordWriterDetails(
						recordName, 
						newBufferedWriter(Conversion.replace(outFileName, recordVariable, recordName).toString())));
			}
		}
		return recordList;
	}

	/**
	 * @param outputCharacterSet the outputCharacterset to set
	 */
	@Override
	public final CobolToCsvBldr setOutputCharacterSet(String outputCharacterSet) {
		this.outputCharacterSet = outputCharacterSet;
		return this;
	}


	@Override
	public final CobolToCsvBldr addRecordDetails(String recordName, BufferedWriter writter) {
		recordList.add(new RecordWriterDetails(recordName, writter));
		return this;
	}

	/**
	 * @return the csvWriter
	 */
	@Override
	public final BufferedWriter getCsvWriter() {
		if (csvWriter == null && outFileName != null && recordVariable == null) {
			try {
				csvWriter = newBufferedWriter(outFileName);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return csvWriter;
	}


	private BufferedWriter newBufferedWriter(String fileName) throws IOException {
		if (outputCharacterSet != null && outputCharacterSet.length() > 0) {
			try {
				return new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName), outputCharacterSet));
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
		}
		return new BufferedWriter(new FileWriter(fileName));
	}


	/**
	 * @param csvWriter the csvWriter to set
	 */
	@Override
	public final CobolToCsvBldr setCsvWriter(BufferedWriter csvWriter) {
		this.csvWriter = csvWriter;
		return this;
	}


	/**
	 * @return the reader
	 */
	@Override
	public final AbstractLineReader getLineReader() {
		return lineReader;
	}

	/**
	 * @param reader the reader to set
	 */
	@Override
	public final CobolToCsvBldr setLineReader(AbstractLineReader reader) {
		this.lineReader = reader;
		return this;
	}
	
	
	@Override
	public CobolToCsvBldr setOutputFile(String filename) {
		this.outFileName = filename;
		return this;
	}

	@Override
	public CobolToCsvBldr setOutputFile(String filename, String recordVariable) {
		this.outFileName = filename;
		this.recordVariable = recordVariable;
		if (recordVariable != null && recordVariable.length() > 0 && ! filename.contains(recordVariable)) {
			throw new RecordException("The filename must contain the record-variable: " + recordVariable);
		}
		return this;
	}

	/**
	 * @return the outFileName
	 */
	public final String getOutFileName() {
		return outFileName;
	}

	/**
	 * @return the recordVariable
	 */
	public final String getRecordVariable() {
		return recordVariable;
	}
    
	/**
	 * @return the updateFieldName
	 */
	@Override
	public final IUpdateFieldName getUpdateFieldName() {
		return updateFieldName;
	}


	/**
	 * @param updateFieldName the updateFieldName to set
	 */
	@Override
	public final CobolToCsvBldr setUpdateFieldName(IUpdateFieldName updateFieldName) {
		this.updateFieldName = updateFieldName;
		return this;
	}


	@Override
    public final void run() throws IOException {
    	new Cobol2CsvMr(this);
    }
}
