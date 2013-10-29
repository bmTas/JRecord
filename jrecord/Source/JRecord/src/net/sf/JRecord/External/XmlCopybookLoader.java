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
package net.sf.JRecord.External;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;



/**
 * This class will load a cb2xml XML Copybook into the Record Layout
 *
 * @author Bruce Martin
 */
public class XmlCopybookLoader implements CopybookLoader {

    private static final int OPT_WRITE_ELEMENT = 1;
    private static final int OPT_REDEFINES = 2;
    private static final int OPT_SAVE = 3;
    private static final int OPT_REDEFINED = 4;

    private static final String STR_YES = "Y";
    private static final String STR_NO  = "N";

    public static final String ATTR_NAME      = "name";
    public static final String ATTR_PICTURE   = "picture";
    public static final String ATTR_NUMERIC   = "numeric";
    public static final String ATTR_REDEFINED = "redefined";
    private static final String ATTR_REDEFINES = "redefines";
    public static final String ATTR_POSITION  = "position";
    public static final String ATTR_LEVEL     = "level";
    private static final String ATTR_OCCURS    = "occurs";
    public static final String ATTR_USAGE     = "usage";
    //private static final String attrDisplayLength = "display-length";
    public static final String ATTR_STORAGE_LENGTH = "storage-length";
    public static final String ATTR_SIGN_SEPARATE  = "sign-separate";
    public static final String ATTR_SIGN_POSITION  = "sign-position";
    public static final String ATTR_SIGNED    = "signed";

    private String copybookName;
    private ArrayList<ExternalField> commonDetails;
    private String redefinedField;
    private boolean foundRedefine;
    private int splitCopybook;

    private int fieldNum;
    private int recordNum;

    private ExternalRecord currentLayout;
    private ExternalRecord parentLayout = null;
    //private ExtendedRecordDB recordDB   = null;
    //private RecordFieldsDB fieldsDB;
    private ExternalRecord groupRecord;
 //   private ChildRecordsDB childRecDB;
    //private static AbsSSLogger logger;


    private int binaryFormat = Convert.FMT_INTEL;
    private Convert numTranslator;
    private String fontName = "";
    private int system = 0;

    private int redefLevel = Integer.MAX_VALUE;

    private int level;
    private ArrayList<String> groupName;

    /**
     * Load a File as a DOM Document
     *
     * @param fileName input file name
     *
     * @return DOM Document
     *
     * @throws IOException error to be handeled by calling program
     * @throws SAXException error to be handeled by calling program
     * @throws ParserConfigurationException error to be handeled by calling program
     */
    public Document fileToDom(String fileName)
	throws IOException, SAXException, ParserConfigurationException {

        DocumentBuilderFactory factory
           		= DocumentBuilderFactory.newInstance();
        return factory.newDocumentBuilder().parse(new File(fileName));
    }


    /**
     * Convert a XML Dom Copybook into the ExternalRecord
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
     * @throws SAXException error to be handeled by calling program
     * @throws ParserConfigurationException error to be handeled by calling program
    */
    public ExternalRecord loadCopyBook(final String copyBookFile,
            						  final int splitCopybookOption,
            						  final int dbIdx,
            						  final String font,
            						  final int binFormat,
            						  final int systemId,
            						  final AbsSSLogger log)
    		throws IOException, SAXException, ParserConfigurationException {

        return loadDOMCopyBook(fileToDom(copyBookFile), Conversion.getCopyBookId(copyBookFile),
                			   splitCopybookOption, dbIdx, font,
       						   binFormat, systemId);

    }


    /**
     * Convert a XML Dom Copybook into a ExternalRecord
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
    public ExternalRecord loadDOMCopyBook(final Document pCopyBookXml,
            							 final String pCopyBook,
            							 final int pSplitCopybook,
            							 final int pDbIdx,
            							 final String font,
               						     final int binFormat,
               						     final int systemId) {
        int i;
        String lCopyBookPref;
        numTranslator = ConversionManager.getInstance().getConverter4code(binFormat) ;

        copybookName = pCopyBook;
        binaryFormat =  numTranslator.getBinaryIdentifier();
        fontName = font;
        system   = systemId;
        parentLayout = null;


        lCopyBookPref = pCopyBook.toUpperCase() + "-";
        this.splitCopybook = pSplitCopybook;

        this.redefinedField = "";
        this.commonDetails  = new ArrayList<ExternalField>();
        this.foundRedefine  = false;
        this.fieldNum       = 0;
        this.recordNum      = 1;

        Element element = /*(Element)*/ pCopyBookXml.getDocumentElement();

        allocDBs(pDbIdx);

        switch (pSplitCopybook) {
        case SPLIT_NONE:   /* split copybook on first redefine*/
            createRecord(pCopyBook, pCopyBook, STR_YES);

            insertXMLcopybook(lCopyBookPref, element);
            break;
        case SPLIT_REDEFINE:
        	scanCopybook4RedefLevel(element);
        	// Deliberate Fall through
        default:
            insertXMLcopybook(lCopyBookPref, element);

            //System.out.println(" ->> " + foundRedefine + " " + commonDetails.size());
            if ((! foundRedefine) && (commonDetails.size() > 0)) {
                createRecord(pCopyBook, pCopyBook, STR_YES);

                for (i = 1; i < commonDetails.size(); i++) {
                    insertRecordField(commonDetails.get(i));
                }
            }
        }

        commonDetails = null;

        if (parentLayout == null) {
        	parentLayout = currentLayout;
        }
        parentLayout.dropFiller();

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

        return parentLayout;
    }

    private boolean isBinaryRec(ExternalRecord rec) {
    	boolean ret = false;
    	TypeManager m = TypeManager.getInstance();

    	try {
	    	for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
	    		ret = ret || m.getType(rec.getRecordField(i).getType()).isBinary();
	    	}
    	} catch (Exception e) {
			System.out.println("Error checking for binary field Types");
		}

    	return ret;
    }

    private int getRecLength(ExternalRecord rec) {
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
                if (!childElement.getAttribute(ATTR_LEVEL).equals("88")) {
                	checkRedef(childElement);
                    scanCopybook4RedefLevel(childElement);
                }
            }
        }
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

       if (element.hasAttribute(ATTR_NAME))  {
    	   try {
    		   int levelNum = getIntAttribute(element, ATTR_LEVEL);
	    	   if (levelNum > 0
	    	   &&  levelNum < redefLevel
	    	   &&  getStringAttribute(element, ATTR_REDEFINED).equals("true")) {
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
    	groupName = new ArrayList<String>();
    	groupName.add(".");

    	insertXMLcopybook(copyBookPref, element, 0, "");
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
            					   final String nameSuffix) {

        String newSuffix;
        NodeList lNodeList = element.getChildNodes();
        level += 1;

        for (int i = 0; i < lNodeList.getLength(); i++) {
            org.w3c.dom.Node node = lNodeList.item(i);
            if (node.getNodeType() == org.w3c.dom.Node.ELEMENT_NODE) {
                Element childElement = (Element) node;
                if (!childElement.getAttribute(ATTR_LEVEL).equals("88")) {
                   if (childElement.hasAttribute(ATTR_OCCURS)) {
                        int childOccurs = getIntAttribute(childElement, ATTR_OCCURS);
                        int length = getIntAttribute(childElement, ATTR_STORAGE_LENGTH);

                        for (int j = 0; j < childOccurs; j++) {
                            if (nameSuffix.equals("")) {
                                newSuffix = Integer.toString(j);
                            } else {
                                newSuffix = nameSuffix + ", " + j;
                            }

                            insertElement(childElement, copyBookPref, newSuffix, basePosition + j * length);
                            insertXMLcopybook(copyBookPref,  childElement, basePosition + j * length, newSuffix);
                        }
                    } else {
                        insertElement(childElement, copyBookPref, nameSuffix, basePosition);
                        insertXMLcopybook(copyBookPref, childElement, basePosition, nameSuffix);
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
            					 	int posBase) {

       boolean print;
       int opt;

       if (element.hasAttribute(ATTR_NAME))  {
           String lName = getStringAttribute(element, ATTR_NAME);
           String lOrigName = lName;
           String usage = "";

           boolean lIsNumeric = getStringAttribute(element, ATTR_NUMERIC).equals("true");

           if (! "".equals(nameSuffix)) {
               lName += " (" + nameSuffix + ")";
           }

           if (lName.toUpperCase().startsWith(copyBookPref)) {
               lName = lName.substring(copyBookPref.length());
           }

           if (element.hasAttribute(ATTR_USAGE)) {
        	   usage = element.getAttribute(ATTR_USAGE);
           }
           /*print = ((! "filler".equalsIgnoreCase(lOrigName))
                   &&  element.hasAttribute(ATTR_PICTURE));*/
           print = element.hasAttribute(ATTR_PICTURE)
           			|| "computational-1".equals(usage) || "computational-2".equals(usage);
           opt = OPT_WRITE_ELEMENT;
           switch (splitCopybook) {
          	  case SPLIT_REDEFINE:
          	      if (foundRedefine) {
	           	     if (getStringAttribute(element, ATTR_REDEFINES).equals(redefinedField)) {
          	            opt = OPT_REDEFINES;
                     }
          	      } else {
          	          opt = OPT_SAVE;
          	          try {
	          	          if (redefLevel == getIntAttribute(element, ATTR_LEVEL)
		          	      &&  getStringAttribute(element, ATTR_REDEFINED).equals("true")) {
		          	          opt = OPT_REDEFINED;
	          	          }
          	          } catch (Exception e) {
          	          }
           	      }
           	  break;
          	  case SPLIT_01_LEVEL:
          	      if (getStringAttribute(element, ATTR_LEVEL).equals("01")) {
        	          opt = OPT_REDEFINED;
          	          if (foundRedefine) {
          	        	  opt = OPT_REDEFINES;
          	          }
           	      }
              default:
           }

           if (! print && level > 0) {
        	   String s = groupName.get(level - 1) + lName + ".";

        	   if (groupName.size() > level) {
        		   groupName.set(level, s);
        	   } else {
        		   groupName.add(s);
        	   }
           }

           switch (opt) {
              case OPT_WRITE_ELEMENT:
                  if (print) {
                      insertRecordField(convertElement2Field(lName, lIsNumeric, posBase, element));
                  }
           	  break;
           	  case OPT_REDEFINED:
                  redefinedField = lOrigName;
                  insertCommonFields(copyBookPref, lName, true);

                  foundRedefine = true;
                  if (print) {
                      insertRecordField(convertElement2Field(lName, lIsNumeric, posBase, element));
                  }
           	  break;
           	  case OPT_REDEFINES:
                  insertCommonFields(copyBookPref, lName, false);
                  if (print) {
                      insertRecordField(convertElement2Field(lName, lIsNumeric, posBase, element));
                  }
           	  break;
           	  case OPT_SAVE:
                  if (print) {
                      commonDetails.add(convertElement2Field(lName, lIsNumeric, posBase, element));
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
        ExternalRecord rec;
        //String recordTypeField = "";
        String name;
        int start = 0;

        if (splitCopybook == SPLIT_01_LEVEL) {
            start = 1;
        }

        if (first) {
            int rt = Constants.rtGroupOfRecords;
            if (binaryFormat == Convert.FMT_MAINFRAME
            ||  binaryFormat == Convert.FMT_BIG_ENDIAN) {
                rt = Constants.rtGroupOfBinaryRecords;
            }

            groupRecord = ExternalRecord.getNullRecord(copybookName,
                    								 rt,
                    								 fontName);

            groupRecord.setListChar(STR_YES);
            groupRecord = getUpdatedRecord(copybookName, groupRecord, false);
            //System.out.println("  " + groupRecord.getRecordType());
        }


//        name = copyBookPref.trim() + recordName.trim();
        name = recordName.trim();
        if (!copyBookPref.endsWith("-") && !copyBookPref.endsWith("_")) {
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
    private ExternalRecord createRecord(String copyBook,
            							String recordName,
            							String listChar) {
        int rt = Constants.rtRecordLayout;
        if (binaryFormat == Convert.FMT_MAINFRAME
        ||  binaryFormat == Convert.FMT_BIG_ENDIAN) {
            rt = Constants.rtBinaryRecord;
        }

        ExternalRecord rec = ExternalRecord.getNullRecord(recordName,
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
    private ExternalRecord getUpdatedRecord(String copyBook,
            					   ExternalRecord rec,
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
    protected void updateRecord(String copyBook,
			   ExternalRecord rec,
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
            								Element element) {
        int iType = Type.ftChar;
        String usage = getStringAttribute(element, ATTR_USAGE);

        if (isNumeric) {
            String picture = getStringAttribute(element, ATTR_PICTURE).toUpperCase();
            String signed = getStringAttribute(element, ATTR_SIGNED);
            String signSeparate = getStringAttribute(element, ATTR_SIGN_SEPARATE);
            String signPosition = getStringAttribute(element, ATTR_SIGN_POSITION);
            iType = numTranslator.getTypeIdentifier(usage, picture, "true".equals(signed),
            		"true".equals(signSeparate), signPosition);

//            if (iType >= 0) {
//            } else if ("true".equals(signed) ||  picture.startsWith("S")) {
//                if ("true".equals(signSeparate)) {
//                    if ("leading".equals(signPosition)) {
//                        iType = Type.ftSignSeparateLead;
//                    } else {
//                        iType = Type.ftSignSeparateTrail;
//                    }
//                } else {
//                    if (binaryFormat == Convert.FMT_MAINFRAME) {
//                      iType = Type.ftZonedNumeric;
//                    } else {
//                      iType = Type.ftFjZonedNumeric;
//                    }
//                }
//            } else {
//                iType = Type.ftAssumedDecimalPositive;
//            }
        } else if ("null-padded".equals(usage)) {
            iType = Type.ftCharNullPadded;
        } else if ("null-terminated".equals(usage)) {
            iType = Type.ftCharNullTerminated;
        } else if ("right-just".equals(usage)) {
            iType = Type.ftCharRightJust;
        }

        ExternalField externalField = new ExternalField(
        	        getIntAttribute(element, ATTR_POSITION) + base,
        	        getIntAttribute(element, ATTR_STORAGE_LENGTH),
        	        name,
        	        "",
        	        iType,
        	        calculateDecimalSize(iType, getStringAttribute(element, ATTR_PICTURE)),
        	        Constants.FORMAT_DEFAULT,
        	        "",
        	        "",
        	        getStringAttribute(element, ATTR_NAME),
        	        fieldNum++
        	  	 );

        if (level > 1) {
        	externalField.setGroup(groupName.get(level - 1));
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
     * Calculate the size of decimal portion
     * @param pFormat Cobol Picture
     * @return decimal size
     */
    private int calculateDecimalSize(int type, String pFormat) {
        int lRet = 0;
        int lPos, lNum, lBracketOpenPos, lBracketClosePos, decimalPos;
        String lDecimalStr;
        String lNumStr;
        String format = "";
        char decimalPnt = '.';
        Type t = TypeManager.getInstance().getType(type);
        if (pFormat != null) {
            format = pFormat.toUpperCase();
        }
        if (t != null) {
        	decimalPnt = t.getDecimalChar();
        }

        decimalPos = format.indexOf(decimalPnt);
        if (decimalPos != format.lastIndexOf(decimalPnt)) {
        	decimalPos = -1;
        }
        lPos = Math.max(format.indexOf("V"), decimalPos);
        if (lPos >= 0) {
            lDecimalStr      = format.substring(lPos + 1);
            lBracketOpenPos  = lDecimalStr.indexOf("(");
            lBracketClosePos = lDecimalStr.indexOf(")");

            if (lBracketOpenPos >= 0 && lBracketClosePos >= lBracketOpenPos) {
                try {
                    lNumStr = lDecimalStr.substring(lBracketOpenPos + 1,
                            						lBracketClosePos);

                    lNum = Integer.parseInt(lNumStr);
                    lRet = lNum + lDecimalStr.length() - (lBracketClosePos - lBracketOpenPos) - 2;

                } catch (Exception e) {
                    /* no action*/
                }
            } else {
                lRet = lDecimalStr.length();
            }
        }

        //System.out.println("===> " + format + "\t" + lRet);

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
}