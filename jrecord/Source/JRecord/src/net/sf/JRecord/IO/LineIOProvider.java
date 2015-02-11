/*
 * @Author Bruce Martin
 * Created on 29/08/2005
 *
 * Purpose:
 *
 * Modification log:
 * On 2006/06/28 by Jean-Francois Gagnon:
 *    - Added a Fujitsu Variable Length Line IO Provider
 *
 * # Version 0.60 Bruce Martin 2007/02/16
 *   - Started work on seperating Record section out, so removing
 *     all reference to the Common module and used a new Constants
 *     module
 */
package net.sf.JRecord.IO;

import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.ByteIO.AbstractByteWriter;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.ByteIO.ByteTextReader;
import net.sf.JRecord.ByteIO.CsvByteReader;
import net.sf.JRecord.ByteIO.IByteReader;
import net.sf.JRecord.Common.AbstractManager;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.Details.CharLineProvider;
import net.sf.JRecord.Details.DefaultLineProvider;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.Details.XmlLineProvider;
import net.sf.JRecord.charIO.CsvCharReader;
import net.sf.JRecord.charIO.ICharReader;
import net.sf.JRecord.charIO.StandardCharReader;

/**
 * LineIOprovider - This class returns a LineIO class appropriate for
 * the supplied file structure. All the Files Structures are available as
 * Constants.IO_*
 *
 * <pre>
 * <b>Usage:</b>
 *
 *         CopybookLoader loader = <font color="brown"><b>new</b></font> RecordEditorXmlLoader();
 *         ExternalRecord extlayout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, <font color="brown"><b>null</b></font>);
 *
 *         LayoutDetail layout = extlayout.asLayoutDetail();
 *         AbstractLineWriter writer = <b>LineIOProvider.getInstance()</b>.getLineWriter(layout.getFileStructure());
 * </pre>
 *
 * @author Bruce Martin
 * @version 0.55
 *
 */
public class LineIOProvider implements AbstractManager {

    private static LineIOProvider ioProvider = null;
    private LineProvider provider;
    private LineProvider charLineProvider = new CharLineProvider();
    private XmlLineProvider xmlProvider = null;

    private static final int numberOfEntries;
    private static String[] names = new String [50] ;
    private static String[] externalNames = new String [50] ;
    private static int[] keys = new int[50];

    
    /**
     * Was used in the RecordEditor, not needed for jrecord
     */
    @Deprecated
    public static final int[] FILE_STRUCTURE_ID = {
    	Constants.IO_DEFAULT,
    	Constants.IO_FIXED_LENGTH,
    	Constants.IO_BINARY,
    	Constants.IO_VB,
    	Constants.IO_VB_DUMP,
    	Constants.IO_VB_FUJITSU,
    	Constants.IO_VB_OPEN_COBOL};
    /**
     * Was used in the RecordEditor, not needed for jarecord
     */
    @Deprecated
    public static final String[] FILE_STRUCTURE_NAME;

   static {
	   String rdDefault = "Default";
	   String rdFixed = "Fixed Length Binary";
	   String rdLineBin = "Line based Binary";
	   String rdVb = "Mainframe VB (rdw based) Binary";
	   String rdVbDump = "Mainframe VB Dump: includes Block length";
	   String rdOcVb = "Open Cobol VB";

	   FILE_STRUCTURE_NAME = new String[] {
			   rdDefault,
			   rdFixed,
			   rdLineBin,
			   rdVb,
			   rdVbDump,
		        "Fujitsu Cobol VB",
		        rdOcVb
		    };

		int i = 0;

		keys[i] = Constants.IO_DEFAULT;				externalNames[i] = "Default";           		names[i++] = rdDefault;
		keys[i] = Constants.IO_TEXT_LINE;			externalNames[i] = "Text";              		names[i++] = "Text IO";
	   	keys[i] = Constants.IO_BIN_TEXT;			externalNames[i] = "Byte_Text";         		names[i++] = "Text IO (byte Based)";
	   	keys[i] = Constants.IO_UNICODE_TEXT;		externalNames[i] = "Text_Unicode";      		names[i++] = "Text IO (Unicode)";
	   	keys[i] = Constants.IO_FIXED_LENGTH;		externalNames[i] = "Fixed_Length";          	names[i++] = "Fixed Length Char";
	   	keys[i] = Constants.IO_FIXED_LENGTH_CHAR;	externalNames[i] = "Fixed_Length_Char";        	names[i++] = rdFixed;
		keys[i] = Constants.IO_BINARY; 				externalNames[i] = "Binary";             		names[i++] = rdLineBin;
		keys[i] = Constants.IO_VB;					externalNames[i] = "Mainframe_VB";				names[i++] = rdVb;
		keys[i] = Constants.IO_VB_DUMP;				externalNames[i] = "Mainframe_VB_As_RECFMU";	names[i++] = rdVbDump;
		keys[i] = Constants.IO_VB_FUJITSU;			externalNames[i] = "FUJITSU_VB";            	names[i++] = "Fujitsu Variable Binary";
		keys[i] = Constants.IO_VB_OPEN_COBOL;		externalNames[i] = "Open_Cobol_VB"; 			names[i++] = rdOcVb;
		
	    keys[i] = Constants.IO_CSV;					externalNames[i] = "CSV_EMBEDDED_CR"; 	 	names[i++] = "Csv Embedded Cr";
	    keys[i] = Constants.IO_UNICODE_CSV;			externalNames[i] = "UNICODE_CSV_EMBEDDED_CR"; names[i++] = "Unicode Csv Embedded Cr";
	    keys[i] = Constants.IO_NAME_1ST_LINE;		externalNames[i] = "CSV_NAME_1ST_LINE";  	names[i++] = "Csv Name on 1st line";
	    keys[i] = Constants.IO_CSV_NAME_1ST_LINE;	externalNames[i] = "CSV_NAME_1ST_LINE_EMBEDDED_CR";  	names[i++] = "Csv Name on 1st line (Embedded Cr)";
	    keys[i] = Constants.IO_UNICODE_NAME_1ST_LINE;externalNames[i] = "UNICODE_CSV_NAME_1ST_LINE_";  	names[i++] = "Unicode Name on 1st line";
	    keys[i] = Constants.IO_UNICODE_CSV_NAME_1ST_LINE;externalNames[i] = "UNICODE_CSV_NAME_1ST_LINE_EMBEDDED_CR";  	names[i++] = "Unicode Name on 1st line (Embedded Cr)";

		keys[i] = Constants.IO_GENERIC_CSV;			externalNames[i] = "CSV_GENERIC";				names[i++] = "Generic CSV (Choose details at run time)";
		keys[i] = Constants.IO_XML_USE_LAYOUT;		externalNames[i] = "XML_Use_Layout"; 			names[i++] = "XML - Existing Layout";
		keys[i] = Constants.IO_XML_BUILD_LAYOUT;	externalNames[i] = "XML_Build_Layout";			names[i++] = "XML - Build Layout";
		keys[i] = Constants.NULL_INTEGER;			externalNames[i] = null;        				names[i++] = null;

		numberOfEntries = i;
    }

    public static final String[] FILE_STRUCTURE = {
        "Default Reader",
        "Fixed Length Binary",
        "Line based Binary",
        "Mainframe VB (rdw based) Binary",
        "Mainframe VB Dump: includes Block length",
        "Fujitsu Cobol VB"
    };


    /**
     * create LineIOprovider class - This class returns IO-Routines
     * appropriate for the Supplied File Structure with a LineProvider
     *
     * @param lineProvider lineProvider to use. Line providers
     * create Lines.
     */
    public LineIOProvider(final LineProvider lineProvider) {
        super();

        provider = lineProvider;
        if (lineProvider == null) {
            provider = new DefaultLineProvider();
        }
    }


    /**
     * create LineIOprovider class - This class returns IO-Routines
     * appropriate for the Supplied File Structure.
     */
    public LineIOProvider() {
        super();
        provider = new DefaultLineProvider();
    }

    /**
	 * @see net.sf.JRecord.Common.AbstractManager#getManagerName()
	 */
	@Override
	public String getManagerName() {
		return "Line_IO_Names";
	}

    /**
     * Gets a Record Reader Class that is appropriate for reading the
     * supplied file-structure
     *
     * @param fileStructure File Structure of the required reader
     *
     * @return line reader
     */
    public AbstractLineReader getLineReader(int fileStructure) {
        return getLineReader(fileStructure, getLineProvider(fileStructure, ""));
    }

    /**
     * Get Reader for Simple-Schema
     * 
     * @param fs file-schema
     * 
     * @return requested reader;
     */
    public AbstractLineReader getLineReader(IBasicFileSchema fs) {
    	int fileStructure = fs.getFileStructure();
		return getLineReader(fs, getLineProvider(fileStructure, fs.getFontName()));
    }
    
    /**
     * Get Reader for Simple-Schema
     * 
     * @param fs file-schema
     * 
     * @return requested reader;
     */
    public AbstractLineReader getLineReader(IBasicFileSchema fs, LineProvider lineProvider) {
    	int fileStructure = fs.getFileStructure();
    	if (lineProvider == null) { 
    		lineProvider = getLineProvider(fileStructure, fs.getFontName());
    	}
		switch (fileStructure) {
		case Constants.IO_BIN_TEXT:		return new LineReaderWrapper(new ByteTextReader(fs.getFontName()));
//       	case Constants.IO_BIN_TEXT:		return new BinTextReader(lLineProvider,  false);
       	case Constants.IO_BIN_NAME_1ST_LINE:
       									return new BinTextReader(lineProvider,  true, new ByteTextReader(fs.getFontName()));
		case Constants.IO_CSV:			return new LineReaderWrapper(getCsvReader(fs, false));
		case Constants.IO_UNICODE_CSV:	return new TextLineReader(lineProvider, false, getCsvCharReader(fs, false));
		case Constants.IO_BIN_CSV:		return new BinTextReader(lineProvider,  false, getCsvReader(fs, false));

		case Constants.IO_UNICODE_CSV_NAME_1ST_LINE: 
			return  new TextLineReader(lineProvider, true, getCsvCharReader(fs, true));
		case Constants.IO_CSV_NAME_1ST_LINE:
		case Constants.IO_BIN_CSV_NAME_1ST_LINE:
			return new BinTextReader(lineProvider,  true, getCsvReader(fs, true));
		default:
			return getLineReader(fileStructure, lineProvider);
		}

//		return getLineReader(fileStructure, lineProvider);
    }

    

	private static IByteReader getCsvReader(IBasicFileSchema schema, boolean namesOnFirstLine) {
		String quote = schema.getQuote();
		if (quote == null || quote.length() == 0) {
			return new ByteTextReader(schema.getFontName());
		}
		return new CsvByteReader(schema.getFontName(), schema.getDelimiter(), quote, quote + quote, namesOnFirstLine);
	}

	private static ICharReader getCsvCharReader(IBasicFileSchema schema, boolean namesOnFirstLine) {
		String quote = schema.getQuote();
		if (quote == null || quote.length() == 0) {
			return new StandardCharReader();
		}
		return new CsvCharReader(schema.getDelimiter(), quote, quote + quote, namesOnFirstLine);
	}

    /**
     * Gets a Record Reader Class that is appropriate for writing the
     * supplied file-structure
     *
     * @param fileStructure File Structure of the required reader
     * @param lineProvider Line-Provider used to create lines
     *
     * @return line reader
     */
    public AbstractLineReader getLineReader(int fileStructure,
            						   LineProvider lineProvider) {
        LineProvider lLineProvider = lineProvider;

        if (lLineProvider == null) {
            lLineProvider = provider;
        }

		//System.out.println(" ~~ IOProvider ~ " + fileStructure + " " + Constants.IO_GENERIC_CSV);

       	switch (fileStructure) {
    	case Constants.IO_BINARY:					return new BinaryLineReader(lLineProvider);

    	case Constants.IO_FIXED_LENGTH_CHAR:		return new FixedLengthTextReader(lLineProvider);

    	case Constants.IO_XML_BUILD_LAYOUT:
       	case Constants.IO_XML_USE_LAYOUT:			return new XmlLineReader(fileStructure == Constants.IO_XML_BUILD_LAYOUT);

       	case Constants.IO_BIN_TEXT:					return new BinTextReader(lLineProvider, false);
       	case Constants.IO_BIN_NAME_1ST_LINE:		return new BinTextReader(lLineProvider, true);

       	case Constants.IO_NAME_1ST_LINE:
       	case Constants.IO_CSV_NAME_1ST_LINE:
       	case Constants.IO_UNICODE_NAME_1ST_LINE:	return new TextLineReader(lLineProvider, true);
       	case Constants.IO_UNICODE_TEXT:				return new TextLineReader(charLineProvider, false);
       	default:
       		AbstractByteReader byteReader
       				= ByteIOProvider.getInstance().getByteReader(fileStructure);

	        if (byteReader != null) {
	            return new LineReaderWrapper(lLineProvider, byteReader);
	        }
	 	}

        return new TextLineReader(lLineProvider, false);
    }


    /**
     * Gets a Record Writer Class
     *
     * @param fileStructure File Structure
     *
     * @return record reader
     */
    public AbstractLineWriter getLineWriter(int fileStructure) {
    	return getLineWriter(fileStructure, null);
    }

    /**
     * Get writer for the schema
     * 
     * @param schema file-schema to get reader for
     * 
     * @return requested writer
     */
    public AbstractLineWriter getLineWriter(IBasicFileSchema schema) {
    	return getLineWriter(schema.getFileStructure(), schema.getFontName());
    }

    /**
     * Gets a Record Writer Class for fileStructure / character-set
     * 
     * @param fileStructure file structure
     * @param charset character set
     * 
     * @return 
     */
    public AbstractLineWriter getLineWriter(int fileStructure, String charset) {


    	switch (fileStructure) {
    	case Constants.IO_BINARY:
    	case Constants.IO_FIXED_LENGTH:				return new BinaryLineWriter();

      	case Constants.IO_FIXED_LENGTH_CHAR:		return new LineWriterWrapperChar(fileStructure);
    	case Constants.IO_XML_BUILD_LAYOUT:
       	case Constants.IO_XML_USE_LAYOUT:			return new XmlLineWriter();

       	case Constants.IO_CSV:
       	case Constants.IO_BIN_CSV:
      	case Constants.IO_BIN_TEXT:					return new BinTextWriter(false);
       	case Constants.IO_BIN_NAME_1ST_LINE:		return new BinTextWriter(true);
       	case Constants.IO_UNICODE_TEXT:
       	case Constants.IO_UNICODE_CSV:				return new TextLineWriter(false);
       	case Constants.IO_NAME_1ST_LINE:
       	case Constants.IO_CSV_NAME_1ST_LINE:      		
       	case Constants.IO_UNICODE_CSV_NAME_1ST_LINE:
       	case Constants.IO_UNICODE_NAME_1ST_LINE:	return new TextLineWriter(true);
       	default:
            AbstractByteWriter byteWriter = ByteIOProvider.getInstance().getByteWriter(fileStructure, charset);

	        if (byteWriter != null) {
	            return new LineWriterWrapper(byteWriter);
	        }
	 	}

        return new TextLineWriter(false);
    }


    /**
     * wether a Copybook file is required
     *
     * @return wether a Copybook file is required
     */
    public boolean isCopyBookFileRequired(int fileStructure) {
    	return ! ( fileStructure == Constants.IO_XML_BUILD_LAYOUT
    			|| fileStructure == Constants.IO_GENERIC_CSV
    			|| fileStructure == Constants.IO_NAME_1ST_LINE
    			|| fileStructure == Constants.IO_CSV_NAME_1ST_LINE);
    }


    /**
     * Convert a file structure to a String
     * @param fileStructure
     * @return Name of the File Structure
     */
    public String getStructureName(int fileStructure) {
    	for (int i = 0; i < keys.length && keys[i] != Constants.NULL_INTEGER; i++) {
    		if (keys[i] == fileStructure) {
    			return externalNames[i];
    		}
    	}
    	return "";
    }

    /**
     * Convert a structure-name to a file-Structure identifier
     * @param name Name of the File Structure
     * @return The file Structure
     */
    public int getStructure(String name) {
    	for (int i = 0; i < keys.length && keys[i] != Constants.NULL_INTEGER; i++) {
    		if (externalNames[i].equalsIgnoreCase(name)) {
    			//System.out.println(" ~~~ getStructure ~ " +  externalNames[i] + " " + keys[i]);
    			return keys[i];
    		}
    	}
    	return Constants.NULL_INTEGER;
    }


    /**
     * Get line provider
     * @return Returns the provider.
     * @deprecated use getLineProvider(fileStructure, String font)
     */
    public LineProvider getLineProvider() {
        return provider;
    }

    public LineProvider getLineProvider(IBasicFileSchema schema) {
    	return getLineProvider(schema.getFileStructure(), schema.getFontName(), schema.isBinary());
    }

    /**
     * Get line provider appropriate to the file Structure / charset
     * @param fileStructure File-Structure
     * @param charset character set
     * @return
     */
    public LineProvider getLineProvider(int fileStructure, String charset) {
    	return getLineProvider(fileStructure, charset, false);
    }
    
    private LineProvider getLineProvider(int fileStructure, String charset, boolean binary) {

    	switch (fileStructure) {
    	case Constants.IO_XML_BUILD_LAYOUT:
    	case Constants.IO_XML_USE_LAYOUT:
       		if (xmlProvider == null) {
       			xmlProvider = new XmlLineProvider();
       		}
       		return xmlProvider;
    	case Constants.IO_FIXED_LENGTH_CHAR:
    	case Constants.IO_UNICODE_CSV:
    	case Constants.IO_UNICODE_CSV_NAME_1ST_LINE:
    	case Constants.IO_UNICODE_NAME_1ST_LINE:
    	case Constants.IO_UNICODE_TEXT:
    		return charLineProvider;
       	}
       	if ((! binary) && Conversion.isMultiByte(charset)) {
       		return charLineProvider;
       	}
        return provider;

    }

//    public final static ArrayList<BasicKeyedField> getFileStructures() {
//    	ArrayList<BasicKeyedField> ret = new ArrayList<BasicKeyedField>();
//    	BasicKeyedField fld;
//
//    	for (int i = 0; i < names.length &&  names[i] != null; i++) {
// 			fld = new BasicKeyedField();
//			fld.key = keys[i];
//			fld.name = names[i];
//			ret.add(fld);
//    	}
//
//    	return ret;
//    }


    /**
	 * @see net.sf.JRecord.Common.AbstractManager#getKey(int)
	 */
	@Override
	public int getKey(int idx) {
		return keys[idx];
	}


	/**
	 * @see net.sf.JRecord.Common.AbstractManager#getName(int)
	 */
	@Override
	public String getName(int idx) {
		return names[idx];
	}


	/**
	 * @see net.sf.JRecord.Common.AbstractManager#getNumberOfEntries()
	 */
	@Override
	public int getNumberOfEntries() {
		return numberOfEntries;
	}


	/**
     * Get an instance of LineIOProvider
     * @return a LineIOProvider
     */
    public static LineIOProvider getInstance() {
        if (ioProvider == null) {
            ioProvider = new LineIOProvider();
        }

        return ioProvider;
    }

    /**
     * Set the system lineIOprovider
     * @param newProvider new IO provider
     */
    public static void setInstance(LineIOProvider newProvider) {
        ioProvider = newProvider;
    }
}
