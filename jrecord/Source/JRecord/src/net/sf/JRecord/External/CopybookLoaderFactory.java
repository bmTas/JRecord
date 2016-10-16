/*
 * @Author Bruce Martin
 * Created on 22/01/2007
 *
 * Purpose:
 * Generate a copybook loader based on the
 * loader id (index) supplied by the calling program.
 * It will also provide a Copybook loader name if required
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

package net.sf.JRecord.External;


import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.Def.AbstractConversion;
import net.sf.JRecord.Log.AbsSSLogger;


/**
 * Generate a copybook loader based on the
 * loader id (index) supplied by the calling program.
 * It will also provide a Copybook loader name if required
 *
 * <p>Examples of use:
 * <pre>
 *       LayoutDetail layout = CopybookLoaderFactory.getInstance().getLayoutRecordEditXml(copybookName, null);
 *
 *       CopybookLoader loader = CopybookLoaderFactory.getInstance()
 *              .getLoader(CopybookLoaderFactory.RECORD_EDITOR_XML_LOADER);
 *       LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
 *
 * </pre>
 *
 * <p>If you are using Cobol look at
 * <pre>
 *      <b>CobolIOProvider.newIOBuilder(..)</b> methods
 * </pre>
 * 
 * @author Bruce Martin
 *
 */
public class CopybookLoaderFactory {

	public static final int CB2XML_LOADER = 0;
	public static final int COBOL_LOADER  = 1;
	public static final int RECORD_EDITOR_XML_LOADER;
	public static final int COMMA_NAMES_1ST_LINE_LOADER;
	public static final int TAB_NAMES_1ST_LINE_LOADER;
	public static final int RECORD_EDITOR_TAB_CSV_LOADER;
	public static final int RECORD_EDITOR_COMMA_CSV_LOADER;

	static {
		int idx = 1;
		if (CobolCopybookLoader.isAvailable()) {
			idx = 2;
		}
		RECORD_EDITOR_XML_LOADER       = idx++;
		COMMA_NAMES_1ST_LINE_LOADER    = idx++;
		TAB_NAMES_1ST_LINE_LOADER      = idx++;
		RECORD_EDITOR_COMMA_CSV_LOADER = idx++;
		RECORD_EDITOR_TAB_CSV_LOADER   = idx++;
	}

	protected static final int NUMBER_OF_LOADERS = 30;

	private String[] loaderName;
	private String[] parameterFile;
	private Class<? extends CopybookLoader>[] loader;

	private int numberLoaded = 0;

	protected int csv1 = 0;
	private   int csv2 = 0;
	private   int xml1 = 0;

	private int recordEditorXml = 0;

	private static CopybookLoaderFactory instance = null;

	/**
	 * Create instance of copybook loader
	 */
	public CopybookLoaderFactory() {
		this(NUMBER_OF_LOADERS);
	}


	/**
	 * Create a Factory with a specified number of loaders
	 * @param maximumNumberOfLoaders maximum number of loaders allowed
	 */
	@SuppressWarnings("unchecked")
	public CopybookLoaderFactory(final int maximumNumberOfLoaders) {
		super();

		loaderName    = new String[maximumNumberOfLoaders];
		parameterFile = new String[maximumNumberOfLoaders];
		loader        = new Class[maximumNumberOfLoaders];

		registerAll();
	}

	/**
	 * Register all loaders
	 */
	protected void registerAll() {

		registerStandardLoaders1();
		registerStandardLoaders2();
	}


	protected final void registerStandardLoaders1() {
		register("cb2xml XML Copybook", XmlCopybookLoader.class, "");
		if (CobolCopybookLoader.isAvailable()) {
			register("Cobol Copybook", CobolCopybookLoader.class, "");
		} else {
	    	register("Empty - Cobol placeholder", XmlCopybookLoader.class, "");
	    }
		recordEditorXml = numberLoaded;
		register("RecordEditor XML Copybook",RecordEditorXmlLoader.class, "");

	}

	protected final void registerStandardLoaders2() {

		csv1 = numberLoaded;
		register("Comma CSV (names first line)", CsvNamesFirstLineFileLoader.class, "");
		csv2   = numberLoaded;
		register("Tab CSV (names first line)", CsvNamesFirstLineFileLoader.Tab.class, "");
		register("RecordEditor Csv Copybook (Comma Seperator)",RecordEditorCsvLoader.Comma.class, "");
		register("RecordEditor Tab Copybook (Tab Seperator)",RecordEditorCsvLoader.Tab.class, "");
		register("DB - CSV extract Copybook", DbCsvCopybookLoader.class, "");
		xml1 = numberLoaded;
		register("XML File", XmlFileLoader.class, "");
	}


	/**
	 * wether the loader can us the input file
	 * @param loaderId loader id to Test
	 * @return wether it can use the input file
	 */
	public final boolean isBasedOnInputFile(int loaderId) {
		boolean ret = (loaderId == xml1) || isCsv(loaderId);
		return ret;
	}

	/**
	 * wether the loader can us the input file
	 * @param loaderId loader id to Test
	 * @return wether it can use the input file
	 */
	public final boolean isCsv(int loaderId) {
		boolean ret = (csv1 <= loaderId) && (loaderId <= csv2);
		return ret;
	}

	public final String getFieldDelim(int loaderId) {
		String ret = "";

		if (loaderId == csv2) {
			ret = "<Tab>";
		} else if (loaderId == csv2 - 1) {
			ret = ",";
		}

		return ret;
	}

	/**
	 * Register a new Copybook loader
	 *
	 * @param name Copybook loader's name
	 * @param loaderClass The loader class
	 * @param propertiesFile properties file
	 */
	public final void register(String name, Class<? extends CopybookLoader> loaderClass, String propertiesFile) {

		loaderName[numberLoaded] = name;
		loader[numberLoaded] =  loaderClass;
		parameterFile[numberLoaded++] = propertiesFile;
	}


	/**
	 * get a specific Copybookloader, typcally used like:
	 * 
	 * <pre>
	 *       CopybookLoader loader = CopybookLoaderFactory.getInstance()
     *              .getLoader(CopybookLoaderFactory.RECORD_EDITOR_XML_LOADER);
     *       LayoutDetail layout = loader.loadCopyBook(copybookName, 0, 0, "", 0, 0, null).asLayoutDetail();
     * </pre>
     * 
     * <p>If you are using Cobol look at
     * <pre>
     *      <b>CobolIOProvider.newIOBuilder(..)</b> methods
	 * </pre>
	 * @param loaderId loader Identifier
	 *
	 * @return requested loader
	 *
	 * @throws IllegalAccessException error
	 * @throws InstantiationException error
	 */
	public CopybookLoader getLoader(int loaderId) throws IllegalAccessException, InstantiationException {
		return loader[loaderId].newInstance();
	}

	/**
	 * Get a Layout from a RecordEditor-Xml file
	 *
	 * @param copyBookFile file that holds the Record Layout (Copybook)
	 * @param log where to write any errors
	 * @return requested Record Layout
	 *
	 * @throws Exception any Error that occurs
	 */
	public final LayoutDetail getLayoutRecordEditXml(String copyBookFile, final AbsSSLogger log)
	throws Exception {
		return getLayout(recordEditorXml, copyBookFile, CopybookLoader.SPLIT_NONE, "", 0, log);
	}

	/**
	 * Get a RecordLayout from an external File
	 *
	 * @param loaderId External Layout format
	 * @param copyBookFile File holding the External Layout
	 * @param splitCopybookOption How to split the copybook (Cobol / Cb2Xml)
	 * @param font Font name
	 * @param binFormat Binary format
	 * @param log Where to write any errors
	 *
	 * @return The Record Layout
	 * @throws Exception Any error that occurs reading the copybook
	 */
	public final LayoutDetail getLayout(int loaderId,
			final String copyBookFile,
            final int splitCopybookOption, final String font,
            final int binFormat,
            final AbsSSLogger log)
	throws Exception {
		return
			getLoader(loaderId).loadCopyBook(
					copyBookFile, splitCopybookOption,
					AbstractConversion.USE_DEFAULT_IDX, font, binFormat, 0, log)
				.asLayoutDetail()
		;
	}


	/**
	 * Get the name of a copybookloader
	 * @param loaderId loader Identifier
	 * @return requested loader name
	 */
	public String getName(int loaderId) {
		return loaderName[loaderId];
	}

	/**
	 * Get a CopybookLoaderFactory
	 * @return Returns a CopybookLoaderFactory
	 */
	public static CopybookLoaderFactory getInstance() {
		if (instance == null) {
			instance = new CopybookLoaderFactory();
		}
		return instance;
	}


	/**
	 * Set the Copbookloader factory
	 * @param newInstance The instance to set.
	 */
	public final static void setInstance(CopybookLoaderFactory newInstance) {
		instance = newInstance;
	}


	/**
	 * Get the number of loaders
	 * @return Returns the numberLoaded.
	 */
	public final int getNumberofLoaders() {
		return numberLoaded;
	}
}
