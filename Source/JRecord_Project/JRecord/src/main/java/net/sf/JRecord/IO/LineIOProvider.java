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
/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.IO;

import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.ByteIO.AbstractByteWriter;
import net.sf.JRecord.ByteIO.BinaryByteWriter;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.ByteIO.ByteTextReader;
import net.sf.JRecord.ByteIO.CsvByteReader;
import net.sf.JRecord.ByteIO.IByteReader;
import net.sf.JRecord.Common.AbstractManager;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.Details.CharLineProvider;
import net.sf.JRecord.Details.DefaultLineProvider;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.Details.XmlLineProvider;
import net.sf.JRecord.External.Def.BasicConversion;
import net.sf.JRecord.charIO.CsvCharReader;
import net.sf.JRecord.charIO.ICharReader;
import net.sf.JRecord.charIO.StandardCharReader;

/**
 * LineIOprovider - This class returns a LineIO class appropriate for
 * the supplied file structure. All the Files Structures are available as
 * Constants.IO_*.
 * 
 * <b>Note:</b> This is part of the "old JRecord Interface". 
 * Most users  will be better off using {@link net.sf.JRecord.net.sf.JRecord.JRecordInterface1} to 
 * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes.
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

    private static final LineProvider CHAR_LINE_PROVIDER = new CharLineProvider();
    private static final LineProvider DEFAULT_PROVIDER   = new DefaultLineProvider();
    private static LineIOProvider ioProvider = null;
    
    
    private LineProvider provider = DEFAULT_PROVIDER;
    private XmlLineProvider xmlProvider = null;

    /**
     * Was used in the RecordEditor, not needed for jrecord
     */
    @Deprecated
    public static final int[] FILE_STRUCTURE_ID = {
    	Constants.IO_DEFAULT,
    	Constants.IO_FIXED_LENGTH,
    	Constants.IO_BINARY_IBM_4680,
    	Constants.IO_VB,
    	Constants.IO_VB_DUMP,
    	Constants.IO_VB_FUJITSU,
    	Constants.IO_VB_GNU_COBOL};
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
	   String rdOcVb = "GNU Cobol VB";

	   FILE_STRUCTURE_NAME = new String[] {
			   rdDefault,
			   rdFixed,
			   rdLineBin,
			   rdVb,
			   rdVbDump,
		        "Fujitsu Cobol VB",
		        rdOcVb
		    };
    }

   @Deprecated
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
            provider = DEFAULT_PROVIDER;
        }
    }


    /**
     * create LineIOprovider class - This class returns IO-Routines
     * appropriate for the Supplied File Structure.
     */
    public LineIOProvider() {
        super();
        provider = DEFAULT_PROVIDER;
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
     * @deprecated use {@link net.sf.JRecord.JRecordInterface1} to create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes.
     * or use {@link LineIOProvider#getLineReader(IBasicFileSchema)}
     */
    public AbstractLineReader getLineReader(int fileStructure) {
        return getLineReader(fileStructure, getLineProvider(fileStructure, null));
    }

    /**
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes.
     * 
     * 
     * Get Reader for Simple-Schema
     * 
     *  
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes.
     * 
     * @param fs file-schema
     * 
     * @return requested reader;
     */
    public AbstractLineReader getLineReader(IBasicFileSchema fs) {
		return getLineReader(fs, getLineProvider(fs.getFileStructure(), fs.getFontName()));
    }
    
    /**
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes.
     * 
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
		case Constants.IO_TEXT_BYTE_ENTER_FONT:
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
		String quote = schema.getQuoteDetails().asString();
		if (quote == null || quote.length() == 0) {
			return new ByteTextReader(schema.getFontName());
		}
		return new CsvByteReader(schema.getFontName(), 
				schema.getDelimiterDetails().asBytes(), schema.getQuoteDetails().asBytes(), 
				quote + quote, namesOnFirstLine);
	}

	private static ICharReader getCsvCharReader(IBasicFileSchema schema, boolean namesOnFirstLine) {
		String quote = schema.getQuoteDetails().asString();;
		if (quote == null || quote.length() == 0) {
			return new StandardCharReader();
		}
		return new CsvCharReader(schema.getDelimiterDetails().asString(), quote, quote + quote, namesOnFirstLine);
	}

    /**
     * Gets a Record Reader Class that is appropriate for writing the
     * supplied file-structure
     *
     * @param fileStructure File Structure of the required reader
     * @param lineProvider Line-Provider used to create lines
     *
     * @return line reader
     * @deprecated use use {@link net.sf.JRecord.JRecordInterface1} to create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes
     * or {@link LineIOProvider#getLineReader(IBasicFileSchema)}
     */
    public AbstractLineReader getLineReader(int fileStructure,
            						   LineProvider lineProvider) {
        LineProvider lLineProvider = lineProvider;

        if (lLineProvider == null) {
            lLineProvider = provider;
        }

		//System.out.println(" ~~ IOProvider ~ " + fileStructure + " " + Constants.IO_GENERIC_CSV);

       	switch (fileStructure) {
    	case Constants.IO_CONTINOUS_NO_LINE_MARKER:	return new ContinuousLineReader(lLineProvider);
    	case Constants.IO_BINARY_IBM_4680:			return new Binary4680LineReader(lLineProvider);

		case Constants.IO_FIXED_CHAR_ENTER_FONT:
    	case Constants.IO_FIXED_LENGTH_CHAR:		return new FixedLengthTextReader(lLineProvider);

    	case Constants.IO_XML_BUILD_LAYOUT:
       	case Constants.IO_XML_USE_LAYOUT:			return new XmlLineReader(fileStructure == Constants.IO_XML_BUILD_LAYOUT);

		case Constants.IO_TEXT_BYTE_ENTER_FONT:
       	case Constants.IO_BIN_TEXT:					return new BinTextReader(lLineProvider, false);
       	case Constants.IO_BIN_NAME_1ST_LINE:		return new BinTextReader(lLineProvider, true);

       	case Constants.IO_NAME_1ST_LINE:
       	case Constants.IO_CSV_NAME_1ST_LINE:
       	case Constants.IO_UNICODE_NAME_1ST_LINE:	return new TextLineReader(lLineProvider, true);
       	case Constants.IO_UNICODE_TEXT:				return new TextLineReader(CHAR_LINE_PROVIDER, false);
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
     * @deprecated  use {@link net.sf.JRecord.JRecordInterface1} to create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes
     * or {@link LineIOProvider#getLineWriter(IBasicFileSchema)}
     */
    public AbstractLineWriter getLineWriter(int fileStructure) {
    	return getLineWriter(fileStructure, null);
    }

    /** 
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes.

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
     *  
     * <b>Note:</b> This is part of the "old JRecord Interface". 
     * Most users  will be better off using {@link net.sf.JRecord.JRecordInterface1} to 
     * create {@link net.sf.JRecord.def.IO.builders.IIOBuilder} classes.
     *
     * Gets a Record Writer Class for fileStructure / character-set,
     * This method will probably get depreciated in favour of {@link LineIOProvider#getLineWriter(IBasicFileSchema)} 
     * 
     * @param fileStructure file structure
     * @param charset character set
     * 
     * @return requested writer
     */
    public AbstractLineWriter getLineWriter(int fileStructure, String charset) {


    	switch (fileStructure) {
    	case Constants.IO_CONTINOUS_NO_LINE_MARKER:	return new ContinuousLineWriter();
    	case Constants.IO_BINARY_IBM_4680:			return new LineWriterWrapper(new BinaryByteWriter());
    	
    	case Constants.IO_FIXED_BYTE_ENTER_FONT:
    	case Constants.IO_FIXED_LENGTH:				return new FixedLengthWriter();

    	case Constants.IO_FIXED_CHAR_ENTER_FONT:
      	case Constants.IO_FIXED_LENGTH_CHAR:		return new LineWriterWrapperChar(fileStructure);
    	case Constants.IO_XML_BUILD_LAYOUT:
       	case Constants.IO_XML_USE_LAYOUT:			return new XmlLineWriter();

    	case Constants.IO_TEXT_BYTE_ENTER_FONT:
       	case Constants.IO_CSV:
       	case Constants.IO_BIN_CSV:
      	case Constants.IO_BIN_TEXT:					return new BinTextWriter(false);
       	case Constants.IO_BIN_NAME_1ST_LINE:		return new BinTextWriter(true);
    	case Constants.IO_TEXT_CHAR_ENTER_FONT:
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
    	return BasicConversion.getStructureName(fileStructure);
    }

    /**
     * Convert a structure-name to a file-Structure identifier
     * @param name Name of the File Structure
     * @return The file Structure
     */
    public int getStructure(String name) {
    	return BasicConversion.getStructure(name);
    }


    /**
     * Get line provider
     * @return Returns the provider.
     * @deprecated use {@link LineIOProvider#getLineProvider(IBasicFileSchema)}
     * or {@link LineIOProvider#getLineProvider(int, String, boolean)}
     */
    public LineProvider getLineProvider() {
        return provider;
    }

    public LineProvider getLineProvider(IBasicFileSchema schema) {
    	return getLineProvider(schema.getFileStructure(), schema.getFontName(), schema.useByteRecord());
    }

    /**
     * Get line provider appropriate to the file Structure / charset
     * @param fileStructure File-Structure
     * @param charset character set
     * @return requested Line Structure
     * @deprecated use {@link LineIOProvider#getLineProvider(IBasicFileSchema)}
     * or {@link LineIOProvider#getLineProvider(int, String, boolean)}
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
//    	case Constants.IO_FIXED_LENGTH_CHAR:
//    	case Constants.IO_UNICODE_CSV:
//    	case Constants.IO_UNICODE_CSV_NAME_1ST_LINE:
//    	case Constants.IO_UNICODE_NAME_1ST_LINE:
//    	case Constants.IO_UNICODE_TEXT:
//    		return CHAR_LINE_PROVIDER;
    		
    	case Constants.IO_FIXED_BYTE_ENTER_FONT:
    	case Constants.IO_TEXT_BYTE_ENTER_FONT:
       	case Constants.IO_FIXED_LENGTH:
    		return DEFAULT_PROVIDER;
//    	case Constants.IO_VB:	
//    	case Constants.IO_VB_DUMP:	
//    	case Constants.IO_VB_FUJITSU:	
//    	case Constants.IO_VB_GNU_COBOL:	
//    		return DEFAULT_PROVIDER;
      	}
    	if (CommonBits.getLineType(fileStructure) == CommonBits.LT_TEXT) {
    		return CHAR_LINE_PROVIDER;
    	}
       	if ((! binary) && (charset != null) && Conversion.isMultiByte(charset)) {
       		return CHAR_LINE_PROVIDER;
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
		return BasicConversion.getFileStructureForIndex(idx);
	}


	/**
	 * @see net.sf.JRecord.Common.AbstractManager#getName(int)
	 */
	@Override
	public String getName(int idx) {
		return BasicConversion.getFileStructureNameForIndex(idx);
	}


	/**
	 * @see net.sf.JRecord.Common.AbstractManager#getNumberOfEntries()
	 */
	@Override
	public int getNumberOfEntries() {
		return BasicConversion.getNumberOfFileStructures();
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
