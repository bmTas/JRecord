/*
 * @Author Bruce Martin
 * Created on 6/04/2005
 *
 * Purpose:
 * This class will insert a XML Copybook into the Record Editor DB
 *
 * Modification log:
 * Changes
 * # Version 0.56
 *   - On 2006/06/28 by Jean-Francois Gagnon:
 *    - Added the ability to process copybooks so they get
 *      processed with the Fujitsu Flavor
 *    - Added handling of the Sign Separate type field
 *   - Bruce Martin 2007/01/16
 *     - Changed to use ExtendedRecord to get null records
 *
 * # Version 0.61
 *   - fixed bug of not calculating decimal places correctly
 *     when lower case v used in Cobol Format
 *
 *  # Version 0.61b
 *   - Strip directories when / is used instead of \
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

package net.sf.JRecord.External.base;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.Types.TypeManager;
import net.sf.cb2xml.def.Cb2xmlConstants;



/**
 * This class will load a cb2xml XML Copybook into the Record Layout
 *
 * @author Bruce Martin
 */
public class BaseCb2xmlLoader<XRecord extends BaseExternalRecord<XRecord>>  {

    private static final int OPT_WRITE_ELEMENT = 1;
    private static final int OPT_REDEFINES = 2;
    private static final int OPT_SAVE = 3;
    private static final int OPT_REDEFINED = 4;

    private static final String STR_YES = "Y";
    private static final String STR_NO  = "N";    

    private String copybookName;
    private ArrayList<ExternalField> commonDetails;
    //private List<DependingOn> commonDependingOn;
    private String redefinedField;
    private boolean foundRedefine, 
    				dropCopybookFromFieldNames = CommonBits.isDropCopybookFromFieldNames();
    private boolean saveCb2xml = false;
    private int splitCopybook;

    private int fieldNum;
    private int recordNum;

    private XRecord currentLayout;
    private XRecord parentLayout = null;
    //private ExtendedRecordDB recordDB   = null;
    //private RecordFieldsDB fieldsDB;
    private XRecord groupRecord;
 //   private ChildRecordsDB childRecDB;
    //private static AbsSSLogger logger;


    private int binaryFormat = ICopybookDialects.FMT_INTEL;
    private Convert numTranslator;
    private String fontName = "";
    private int system = 0;

    private int redefLevel = Integer.MAX_VALUE;

    private int level;
    private String splitAtLevel;
    private int positionAdjustment = 0;
//    private ArrayList<String> groupName;
    
//    boolean dropCopybookFromFieldNames = CommonBits.isDropCopybookFromFieldNames();
    
    boolean keepFiller = false;
    private final boolean useJRecordNaming ;
    private final IExernalRecordBuilder<XRecord> recBuilder;
    
    private FieldCreatorHelper fieldHelper;
//    private HashMap<String, String> fieldToNameWithArrayIndexs = new HashMap<String, String>();
//    private HashMap<String, DependingOn> nameToDependDtls = new HashMap<String, DependingOn>();
    
    public void setStackSize(int size) {
    	
    }

    protected BaseCb2xmlLoader(IExernalRecordBuilder<XRecord> recBuilder, boolean useJRecordNaming) {
    	this.recBuilder = recBuilder;
    	this.useJRecordNaming = useJRecordNaming;
    }
    
    /**
     * Load a File as a DOM Document
     *
     * @param fileName input file name
     *
     * @return DOM Document
     *
     * @throws IOException error to be handled by calling program
      */
    public Document fileToDom(String fileName)
	throws IOException, SAXException, ParserConfigurationException {

    	synchronized (this) {	
	        DocumentBuilderFactory factory
	           		= DocumentBuilderFactory.newInstance();
	        return factory.newDocumentBuilder().parse(new File(fileName));
		}
    }
    
    

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.ISetDropCopybookName#setSaveCb2xmlDocument(boolean)
	 */
	public void setSaveCb2xmlDocument(boolean saveCb2xml) {
		this.saveCb2xml = saveCb2xml;
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	public final XRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int binFormat,
			int systemId, AbsSSLogger log) throws IOException, SAXException, ParserConfigurationException {
		
		return loadCopyBook(copyBookFile, splitCopybookOption, dbIdx, font, CommonBits.getDefaultCobolTextFormat(), binFormat, systemId, log);
		
	}


    /* (non-Javadoc)
	 * @see net.sf.JRecord.External.ICopybookLoaderStream#loadCopyBook(java.io.InputStream, java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	public XRecord loadCopyBook(InputStream inputStream,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		try {
			synchronized (this) {	
			    DocumentBuilderFactory factory
			       		= DocumentBuilderFactory.newInstance();
			    Document doc = factory.newDocumentBuilder().parse(inputStream);
				return loadDOMCopyBook(doc, copyBookName, splitCopybook, dbIdx, font, binaryFormat, systemId);
			}
		} catch (SAXException e) {
			throw new IOException(e);
		} catch (ParserConfigurationException e) {
			throw new IOException(e);
		}
	}
	
	

	public XRecord loadCopyBook(Reader reader,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		try {
			synchronized (this) {	
			    DocumentBuilderFactory factory
			       		= DocumentBuilderFactory.newInstance();
			    Document doc = factory.newDocumentBuilder().parse(new InputSource(reader));
				return loadDOMCopyBook(doc, copyBookName, splitCopybook, dbIdx, font, binaryFormat, systemId);
			}
		} catch (SAXException e) {
			throw new IOException(e);
		} catch (ParserConfigurationException e) {
			throw new IOException(e);
		}
	}


	/**
     * Convert a XML Dom Copybook into the XRecord
     *
     * @param copyBookFile Copy Book file Name
     * @param splitCopybookOption wether to split a copy book on a redefine
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return record to be inserted
     *
     * @throws IOException error to be handeled by calling program
    */
    public XRecord loadCopyBook(  final String copyBookFile,
            					  final int splitCopybookOption,
            					  final int dbIdx,
            					  final String font,
            					  final int copybookFormat,
            					  final int binFormat,
            					  final int systemId,
            					  final AbsSSLogger log)
    		throws IOException, SAXException, ParserConfigurationException {

        return loadDOMCopyBook(fileToDom(copyBookFile), Conversion.getCopyBookId(copyBookFile),
                			   splitCopybookOption, dbIdx, font,
       						   binFormat, systemId);

    }


    /**
     * Convert a XML Dom Copybook into a XRecord
     *
     * @param pCopyBookXml copy book XML DOM representation of a copybook
     * @param pCopyBook Copy Book file Name
     * @param pSplitCopybook wether to split a copy book on a redefine
     * @param pDbIdx Database index
     * @param font font name to use
     * @param binFormat binary format to use
     * @param systemId System Identifier
     *
     * @return record to be inserted
     */
    public XRecord loadDOMCopyBook(	final Document pCopyBookXml,
            						final String pCopyBook,
            						final int pSplitCopybook,
            						final int pDbIdx,
            						final String font,
               						final int binFormat,
               						final int systemId) {
    	synchronized (this) {
    		int i;
    		String lCopyBookPref;
    		numTranslator = ConversionManager.getInstance().getConverter4code(binFormat) ;

    		copybookName = pCopyBook;
    		binaryFormat =  numTranslator.getBinaryIdentifier();
    		fontName = font;
    		system   = systemId;
    		parentLayout = null;


            this.fieldHelper = new FieldCreatorHelper(pSplitCopybook, binFormat, useJRecordNaming, pCopyBook, font);
            this.fieldHelper.setDropCopybookFromFieldNames(dropCopybookFromFieldNames);
            this.splitCopybook = pSplitCopybook;

            this.redefinedField = "";
            this.commonDetails  = new ArrayList<ExternalField>();
            this.foundRedefine  = false;
            this.fieldNum       = 0;
            this.recordNum      = 1;
            this.positionAdjustment = 0;

            Element element = /*(Element)*/ pCopyBookXml.getDocumentElement();

            allocDBs(pDbIdx);
            
            lCopyBookPref = fieldHelper.getCopyBookPref();
            splitAtLevel = "1";
    
            switch (pSplitCopybook) {
            case ICobolSplitOptions.SPLIT_NONE:   /* split copybook on first redefine*/
                createRecord(pCopyBook, pCopyBook, STR_YES);

                insertXMLcopybook(lCopyBookPref, element);
                break;
            case ICobolSplitOptions.SPLIT_REDEFINE:
                scanCopybook4RedefLevel(element);
                processCopybook(pCopyBook, lCopyBookPref, element);
                break;
            case ICobolSplitOptions.SPLIT_HIGHEST_REPEATING:
                scanCopybook4Level(element);
                processCopybook(pCopyBook, lCopyBookPref, element);
                break;
            default:
            	String topLevel = getTopLevel(element);
            	if ("1".equals(topLevel)) {
            		processCopybook(pCopyBook, lCopyBookPref, element);
            	} else {
                    createRecord(pCopyBook, pCopyBook, STR_YES);

                    insertXMLcopybook(lCopyBookPref, element);          		
            	}
            }

            commonDetails = null;

            if (parentLayout == null) {
                parentLayout = currentLayout;
            }
            
            if (! keepFiller) {
            	parentLayout.dropFiller();
            }

            boolean multipleRecordLengths = false,
                    binary = false;

            if (parentLayout.getNumberOfRecords() == 0) {
                binary = isBinaryRec(parentLayout);
            } else {
                int len = getRecLength(parentLayout.getRecord(0));
                binary = binary || isBinaryRec(parentLayout.getRecord(0));

                for (i = 1; i < parentLayout.getNumberOfRecords(); i++) {
                    binary = binary || isBinaryRec(parentLayout.getRecord(i));
                    multipleRecordLengths = multipleRecordLengths
                                         || (len != getRecLength(parentLayout.getRecord(i)));
                }
            }
            parentLayout.setFileStructure(numTranslator.getFileStructure(multipleRecordLengths, binary));
            freeDBs(pDbIdx);
       
            if (saveCb2xml) {
            	parentLayout.addCb2xmlDocument(new Cb2xmlDocument(pSplitCopybook, splitAtLevel, pCopyBookXml));
            }

            return parentLayout;
        }
    }
    
    private void processCopybook(String pCopyBook, String lCopyBookPref, Element element) {
        insertXMLcopybook(lCopyBookPref, element);
    	
        //System.out.println(" ->> " + foundRedefine + " " + commonDetails.size());
        if ((! foundRedefine) && (commonDetails.size() > 0)) {
            createRecord(pCopyBook, pCopyBook, STR_YES);

            for (int i = 1; i < commonDetails.size(); i++) {
                insertRecordField(commonDetails.get(i));
            }
        }

    }

    private boolean isBinaryRec(XRecord rec) {
    	boolean ret = false;
    	TypeManager m = TypeManager.getInstance();

    	try {
	    	for (int i = 0; i < rec.getNumberOfRecordFields() && (! ret); i++) {
	    		ret = m.getType(rec.getRecordField(i).getType()).isBinary();
	    	}
    	} catch (Exception e) {
			System.out.println("Error checking for binary field Types");
		}

    	return ret;
    }

    private int getRecLength(XRecord rec) {
    	int ret = 0;

    	try {
	    	for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
	    		ExternalField recordField = rec.getRecordField(i);
				ret = Math.max(ret, recordField.getPos() + recordField.getLen() - 1);
	    	}
    	} catch (Exception e) {
			System.out.println("Error Finding Record Length Types");
		}

    	return ret;
    }
    protected void allocDBs(int pDbIdx) {

    }


    protected void freeDBs(int pDbIdx) {

    }


    /**
     * Scan Copybook for lowest level where that is redefined
     *
     * @param element XML element source
     */
    private void scanCopybook4RedefLevel(final Element element) {

        NodeList lNodeList = element.getChildNodes();

        for (int i = 0; i < lNodeList.getLength(); i++) {
            org.w3c.dom.Node node = lNodeList.item(i);
            if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
                Element childElement = (Element) node;
                if (!childElement.getAttribute(Cb2xmlConstants.LEVEL).equals("88")) {
                	checkRedef(childElement);
                    scanCopybook4RedefLevel(childElement);
                }
            }
        }
    }
    
    
    private void scanCopybook4Level(Element element) {
    	String lvl = getTopLevel(element);
    	if (lvl != null) {
    		splitAtLevel = lvl;
    	}
    }
    
    private String getTopLevel(Element element) {
        NodeList lNodeList = element.getChildNodes();
        while (lNodeList != null && lNodeList.getLength() == 1) {
        	lNodeList = lNodeList.item(0).getChildNodes();
        }
        if (lNodeList != null && lNodeList.getLength() > 0) {
        	org.w3c.dom.Node node = null;
        	int i = 0;
        	
        	while (i < lNodeList.getLength() 
        	   && (node = lNodeList.item(i++)).getNodeType() != org.w3c.dom.Node.ELEMENT_NODE) {};
        	   
            if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
                Element childElement = (Element) node;
                String attrLevel = childElement.getAttribute(Cb2xmlConstants.LEVEL);
				if (! attrLevel.equals("88")) {
					return Conversion.numTrim(attrLevel);
                }
            }
        }
        return null;
    }

    /**
     * insert an XML element (field) into the Field DB.
     *
     * @param element element to be inserted into the DB
     * @param copyBookPref Copybook name
     * @param nameSuffix suffix to be used on field names
     * @param posBase base position
     */
    private void checkRedef(Element element) {

       if (element.hasAttribute(Cb2xmlConstants.NAME))  {
    	   try {
    		   int levelNum = getIntAttribute(element, Cb2xmlConstants.LEVEL);
	    	   if (levelNum > 0
	    	   &&  levelNum < redefLevel
	    	   &&  getStringAttribute(element, Cb2xmlConstants.REDEFINED).equalsIgnoreCase(Cb2xmlConstants.TRUE)) {
		           redefLevel = levelNum;
		       }
    	   } catch (Exception e) {
    	   }
       }
    }


    /**
     * Insert XML Copybook into Record Fields
     *
     * @param copyBookPref copy book name
     * @param element XML element source
     * @param basePosition base position
     * @param nameSuffix Name suffix
     */
    private void insertXMLcopybook(final String copyBookPref,
    							   final Element element) {
    	level = 0;
//    	groupName = new ArrayList<String>();
//    	groupName.add(".");

    	insertXMLcopybook(copyBookPref, element, 0, "", null);
    }


    /**
     * Insert XML Copybook into Record Fields
     *
     * @param copyBookPref copy book name
     * @param element XML element source
     * @param basePosition base position
     * @param nameSuffix Name suffix
     */
    private void insertXMLcopybook(final String copyBookPref,
    							   final Element element,
            					   final int basePosition,
            					   final String nameSuffix,
            					   final DependingOnDtls dependOnParentDtls) {

        String newSuffix;
        NodeList lNodeList = element.getChildNodes();
        level += 1;

        for (int i = 0; i < lNodeList.getLength(); i++) {
            org.w3c.dom.Node node = lNodeList.item(i);
            if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
                Element childElement = (Element) node;
                if (!childElement.getAttribute(Cb2xmlConstants.LEVEL).equals("88")) {
                	
                   if (childElement.hasAttribute(Cb2xmlConstants.OCCURS)) {
                        int childOccurs = getIntAttribute(childElement, Cb2xmlConstants.OCCURS);
                        int length = getIntAttribute(childElement, Cb2xmlConstants.STORAGE_LENGTH);
                        String dependingVar = getStringAttribute(childElement, Cb2xmlConstants.DEPENDING_ON);
                        DependingOn dependOn = null;
                        
                        if (dependingVar.length() > 0) {
                        	ExternalField tmpField = convertElement2Field("xx", false, basePosition, childElement, null);
                        	dependOn = fieldHelper
                        					.dependingOnBuilder()
                        						.setPosition(tmpField.getPos())
                        						.setLength(length)
                        						.setChildOccurs(childOccurs)
                        					.newDependingOn(currentLayout, dependOnParentDtls, dependingVar);
                        			;
                        }

                        int size = 0;
                        if (currentLayout != null && currentLayout.fields != null) {
	                        size = currentLayout.fields.size();
	                        
	                        currentLayout.fields.ensureCapacity(size + childOccurs);
                        }
                        for (int j = 0; j < childOccurs; j++) {
                        	newSuffix = fieldHelper.updateFieldNameIndex(nameSuffix, j);

                            DependingOnDtls dependOnDtls = dependOnParentDtls;
                            if (dependOn != null) {
                            	dependOnDtls = new DependingOnDtls(dependOn, j, dependOnParentDtls);
                            }
                            insertElement(childElement, copyBookPref, newSuffix, basePosition + j * length, dependOnDtls);
                            
                            insertXMLcopybook(copyBookPref,  childElement, basePosition + j * length, newSuffix, dependOnDtls);
                        }
                    } else {
                        insertElement(childElement, copyBookPref, nameSuffix, basePosition, dependOnParentDtls);
                        insertXMLcopybook(copyBookPref, childElement, basePosition, nameSuffix, dependOnParentDtls);
                    }
                }
            }
        }

        level -= 1;
    }

    /**
     * insert an XML element (field) into the Field DB.
     *
     * @param element element to be inserted into the DB
     * @param copyBookPref Copybook name
     * @param nameSuffix suffix to be used on field names
     * @param posBase base posisition
     */
    private void insertElement(	Element element,
            					 String copyBookPref,
            					 String nameSuffix,
            					 	int posBase,
            			DependingOnDtls dependDtls) {

       boolean print;
       int opt;

       if (element.hasAttribute(Cb2xmlConstants.NAME))  {
           String lOrigName = getStringAttribute(element, Cb2xmlConstants.NAME);
           String lName = fieldHelper.createFieldName(lOrigName, nameSuffix);
           String usage = "";
           
           lOrigName = fieldHelper.createOriginalName(lOrigName);

           boolean lIsNumeric = getStringAttribute(element, Cb2xmlConstants.NUMERIC).equalsIgnoreCase(Cb2xmlConstants.TRUE);

           if (element.hasAttribute(Cb2xmlConstants.USAGE)) {
        	   usage = element.getAttribute(Cb2xmlConstants.USAGE);
           }
           print = element.hasAttribute(Cb2xmlConstants.PICTURE)
           			|| Cb2xmlConstants.COMP_1.equals(usage) || Cb2xmlConstants.COMP_2.equals(usage);
           opt = OPT_WRITE_ELEMENT;
           switch (splitCopybook) {
          	  case ICobolSplitOptions.SPLIT_REDEFINE:
          	      if (foundRedefine) {
	           	     if (getStringAttribute(element, Cb2xmlConstants.REDEFINES).equals(redefinedField)) {
          	            opt = OPT_REDEFINES;
                     }
          	      } else {
          	          opt = OPT_SAVE;
          	          try {
	          	          if (redefLevel == getIntAttribute(element, Cb2xmlConstants.LEVEL)
		          	      &&  getStringAttribute(element, Cb2xmlConstants.REDEFINED).equalsIgnoreCase(Cb2xmlConstants.TRUE)) {
		          	          opt = OPT_REDEFINED;
	          	          }
          	          } catch (Exception e) {
          	          }
           	      }
           	  break;
          	  case ICobolSplitOptions.SPLIT_HIGHEST_REPEATING:
          		   if (Conversion.numTrim(getStringAttribute(element, Cb2xmlConstants.LEVEL)).equals(splitAtLevel)) {
          			  positionAdjustment = getIntAttribute(element, Cb2xmlConstants.POSITION) - 1;
         	          opt = foundRedefine ? OPT_REDEFINES : OPT_REDEFINED;
         		  }
          	  break;
          	  case ICobolSplitOptions.SPLIT_01_LEVEL:
          	      if (Conversion.numTrim(getStringAttribute(element, Cb2xmlConstants.LEVEL)).equals(splitAtLevel)) {
        	          opt = foundRedefine ? OPT_REDEFINES : OPT_REDEFINED;
           	      }
              default:
           }

           //System.out.println(level + " " + print + " " + lName);
//           if (level > 0 && level <= groupName.size()) {
//        	   String s = groupName.get(level - 1) + lOrigName + ".";
//
//        	   if (groupName.size() > level) {
//        		   groupName.set(level, s);
//        	   } else {
//        		   groupName.add(s);
//        	   }
//           }
           fieldHelper.updateGroup(opt, lOrigName);

           switch (opt) {
              case OPT_WRITE_ELEMENT:
                  if (print) {
                      insertRecordField(convertElement2Field(lName, lIsNumeric, posBase, element, dependDtls));
                  }
           	  break;
           	  case OPT_REDEFINED:
                  redefinedField = lOrigName;
                  insertCommonFields(copyBookPref, lName, true);

                  foundRedefine = true;
                  fieldHelper.setFoundRedefine(foundRedefine);
                  if (print) {
                      insertRecordField(convertElement2Field(lName, lIsNumeric, posBase, element, dependDtls));
                  }
           	  break;
           	  case OPT_REDEFINES:
                  insertCommonFields(copyBookPref, lName, false);
                  if (print) {
                      insertRecordField(convertElement2Field(lName, lIsNumeric, posBase, element, dependDtls));
                  }
           	  break;
           	  case OPT_SAVE:
                  if (print) {
                      commonDetails.add(convertElement2Field(lName, lIsNumeric, posBase, element, dependDtls));
                  }
          	  break;
           	  default:
           }
        }
    }


    /**
     * Insert Common Record Details
     *
     * @param copyBookPref prefix to remove from field names
     * @param recordName Field Name
     * @param first wether its is the first time called
     */
    private void insertCommonFields(String copyBookPref, String recordName, boolean first) {
        int i;
        XRecord rec;
        //String recordTypeField = "";
        String name;
        int start = 0;

        switch  (splitCopybook) {
        case ICobolSplitOptions.SPLIT_HIGHEST_REPEATING:       
        case ICobolSplitOptions.SPLIT_01_LEVEL:       
            start = 1;
        }

        if (first) {
            int rt = Constants.rtGroupOfRecords;
            if (binaryFormat == ICopybookDialects.FMT_MAINFRAME
            ||  binaryFormat == ICopybookDialects.FMT_BIG_ENDIAN) {
                rt = Constants.rtGroupOfBinaryRecords;
            }

            groupRecord = recBuilder.getNullRecord(copybookName,
                    								 rt,
                    								 fontName);

            groupRecord.setListChar(STR_YES);
            groupRecord = getUpdatedRecord(copybookName, groupRecord, false);
            //System.out.println("  " + groupRecord.getRecordType());
            
            //TODO
            //TODO occurs depending
        }


		if (useJRecordNaming) {
			name = recordName.trim();
    	} else if (copyBookPref.endsWith("-") || copyBookPref.endsWith("_")) {
      	   name = copyBookPref.trim() + recordName.trim();
        } else {
            name = copyBookPref + " " + recordName;
		}
        
        rec = createRecord(copyBookPref + recordNum++,
        				   name,
        				   STR_NO);
        parentLayout.addRecord(rec);

        for (i = start; i < commonDetails.size(); i++) {
            insertRecordField(commonDetails.get(i));
        }

        fieldNum = commonDetails.size() + 1;
        
        List<DependingOn> commonDependingOn = fieldHelper.getCommonDependingOn();
        if (commonDependingOn != null) {
	        for (DependingOn dependOn : commonDependingOn) {
	        	currentLayout.addDependingOn(dependOn);
	        }
        }
    }


    /**
     * Insert / Update a Normal Record record into the record DB
     *
     * @param copyBook Cobol Copybook name
     * @param recordName Record name
     * @param listChar List on Combobox (values Y/N for Yes/No)
     *
     * @return the record just inserted
     */
    @SuppressWarnings("deprecation")
	private XRecord createRecord(String copyBook,
            							String recordName,
            							String listChar) {
        int rt = Constants.rtRecordLayout;
        if (binaryFormat == ICopybookDialects.FMT_MAINFRAME) {
            rt = Constants.rtBinaryRecord;
        }

        XRecord rec = recBuilder.getNullRecord(recordName,
				   				rt,
				   				fontName);
        rec.setListChar(listChar);
        rec.setSystem(system);

        return getUpdatedRecord(copyBook, rec, false);
    }


    /**
     * Update the record details. This method will read current
     * DB details (if there are any) and apply standard updates to
     * the record
     *
     * @param copyBook Copybook name
     * @param rec Record Details
     * @param updateRequired Wether a update of existing record is required
     *
     * @return the record just inserted
     */
    private XRecord getUpdatedRecord(String copyBook,
            					   XRecord rec,
            					   boolean updateRequired) {

        updateRecord(copyBook, rec, updateRequired);

        currentLayout = rec;
        if (parentLayout == null) {
            parentLayout = rec;
        }
        return rec;
    }


    /**
     * Updates record values, provides a hook for
     * updating with the current values on the DB
     *
     * @param copyBook copybook name
     * @param rec record to be updated
     * @param updateRequired wether an update is required
     */
    @SuppressWarnings("deprecation")
	protected void updateRecord(String copyBook,
			   XRecord rec,
			   boolean updateRequired) {

        rec.setSystem(system);
        rec.setCopyBook(copyBook);
        rec.setNew(true);
    }

    /**
     * Insert Record Field Details into the DB
     *
     * @param field source field
     *
     */
    private void insertRecordField(ExternalField field) {

        currentLayout.addRecordField(field);
    }


    /**
     * Converts a DOM element to a Record Field
     *
     * @param name Field Name
     * @param isNumeric weather it is numeric
     * @param base base position (used to handle arrays
     * @param element DOM element
     *
     * @return the Field
     */
    private ExternalField convertElement2Field(
            								String name,
            								boolean isNumeric,
            								int base,
            								Element element,
            								DependingOnDtls dependDtls) {
//        int iType = Type.ftChar;
        String usage = getStringAttribute(element, Cb2xmlConstants.USAGE);
        String just = getStringAttribute(element, Cb2xmlConstants.JUSTIFIED);
        String picture = getStringAttribute(element, Cb2xmlConstants.PICTURE).toUpperCase();
        String signSeparate = getStringAttribute(element, Cb2xmlConstants.SIGN_SEPARATE);
        String signPosition = getStringAttribute(element, Cb2xmlConstants.SIGN_POSITION);
        
        
        int iType = fieldHelper.deriveType(isNumeric, usage, picture, 
        		Cb2xmlConstants.TRUE.equals(signSeparate), signPosition, 
        		just != null && just.length() > 0);
        
//        if (isNumeric) {
//            String picture = getStringAttribute(element, Cb2xmlConstants.PICTURE).toUpperCase();
//            String signed = getStringAttribute(element, Cb2xmlConstants.SIGNED);
//            String signSeparate = getStringAttribute(element, Cb2xmlConstants.SIGN_SEPARATE);
//            String signPosition = getStringAttribute(element, Cb2xmlConstants.SIGN_POSITION);
//            iType = numTranslator.getTypeIdentifier(usage, picture, Cb2xmlConstants.TRUE.equals(signed),
//            		Cb2xmlConstants.TRUE.equals(signSeparate), signPosition);
//
//        } else if ("null-padded".equals(usage)) {
//            iType = Type.ftCharNullPadded;
//        } else if ("null-terminated".equals(usage)) {
//            iType = Type.ftCharNullTerminated;
//        } else {	
//        	String just = getStringAttribute(element, Cb2xmlConstants.JUSTIFIED);
//        	if (just != null && just.length() > 0) {
//               iType = Type.ftCharRightJust;
//        	}
//        }

        ExternalField externalField = new ExternalField(
        	        getIntAttribute(element, Cb2xmlConstants.POSITION) + base - positionAdjustment,
        	        getIntAttribute(element, Cb2xmlConstants.STORAGE_LENGTH),
        	        name,
        	        "",
        	        iType,
        	        fieldHelper.calculateDecimalSize(
        	        		iType, 
        	        		getStringAttribute(element, Cb2xmlConstants.PICTURE),
        	        		getIntAttribute(element, Cb2xmlConstants.SCALE)
        	        ),
        	        Constants.FORMAT_DEFAULT,
        	        "",
        	        "",
        	        getStringAttribute(element, Cb2xmlConstants.NAME),
        	        fieldNum++,
        	        dependDtls
        	  	 );

        if (level > 1 && fieldHelper.getGroupNameSize() > level) {
        	externalField.setGroup(fieldHelper.getGroupName(level - 1));
        }
		return externalField;
    }


    /**
     * Get an integer Attribute
     *
     * @param element source element
     * @param attributeName Attribute Name
     *
     * @return Attribute value
     */
    private int getIntAttribute(Element element, String attributeName) {
        int lRet = 0;

        if (element.hasAttribute(attributeName))  {
            lRet = Integer.parseInt(element.getAttribute(attributeName));
        }

        return lRet;
    }


    /**
	 * @return the system
	 */
	public int getSystem() {
		return system;
	}

	/**
	 * @param system the system to set
	 */
	public void setSystem(int system) {
		this.system = system;
	}

	/**
     * Get a string attribute from a element
     *
     * @param element source element
     * @param attributeName Attribute Name
     *
     * @return Attribute value
     */
    public String getStringAttribute(Element element, String attributeName) {
        String lRet = "";

        if (element.hasAttribute(attributeName))  {
            lRet = element.getAttribute(attributeName);
        }

        return lRet;
    }



    /**
     * Get the font name
     *
     * @return Returns the fontName.
     */
    public String getFontName() {
        return fontName;
    }


    /**
     * Sets the font name to be used when creating a record
     *
     * @param pFontName font name to use
     */
    public void setFontName(String pFontName) {
        fontName = pFontName;
    }


    /**
     * Binary format to use
     *
     * @param pMachine binary format to use
     */
    public void setBinaryFormat(int pMachine) {
        binaryFormat = pMachine;
    }

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.ISetDropCopybookName#setDropCopybookFromFieldNames(boolean)
	 */
	public final void setDropCopybookFromFieldNames(
			boolean dropCopybookFromFieldNames) {
		this.dropCopybookFromFieldNames = dropCopybookFromFieldNames;
		
		if (this.fieldHelper != null) {
			this.fieldHelper.setDropCopybookFromFieldNames(dropCopybookFromFieldNames);
		}
	}



	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.ISetDropCopybookName#setKeepFillers(boolean)
	 */
	public void setKeepFillers(boolean keepFiller) {
		this.keepFiller = keepFiller;
	}
}