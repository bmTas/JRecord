package net.sf.JRecord.External.base;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.External.Def.DependingOn;
import net.sf.JRecord.External.Def.DependingOnDtls;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;


/**
 * Class to with methods to help Create JRecord Field definitions
 * 
 * @author Bruce Martin
 *
 */
public class FieldCreatorHelper {

    private boolean dropCopybookFromFieldNames = CommonBits.isDropCopybookFromFieldNames();


	private List<DependingOn> commonDependingOn;
	private final int splitCopybook;
	private final HashMap<String, String> fieldToNameWithArrayIndexs = new HashMap<String, String>();
    //private BaseExternalRecord<?> currentLayout;
    private boolean foundRedefine = false;
    
    private int position, length, childOccurs;
    //private final boolean useJRecordNaming;
    private final String copyBookPref, font;
    private final Convert numTranslator;
    
    private ArrayList<String> groupName;
    
    int lastEndPos = -1;
    net.sf.JRecord.External.Def.ExternalField lastFiller;
    private int initialLevel = 1;



	public FieldCreatorHelper(int splitCopybook, int dialect, boolean useJRecordNaming, String copyBookPref, String font) {
		this.splitCopybook = splitCopybook;
		//this.useJRecordNaming = useJRecordNaming;
		this.copyBookPref = (useJRecordNaming 	? copyBookPref.toUpperCase() 
												: copyBookPref)
							+ "-";
    	this.font = font;
		this.numTranslator = ConversionManager.getInstance().getConverter4code(dialect) ;
		this.groupName = new ArrayList<String>();
    	this.groupName.add(".");
	}

	 
	public void setParentGroup(String[] parentGroupNames) {
		int level = 1;
		if (parentGroupNames != null) {
			for (String s : parentGroupNames) {
				updateGroup(level++, s);
			}
		}
		initialLevel = level;
	}

	/**
	 * @return the initialLevel
	 */
	public int getInitialLevel() {
		return initialLevel;
	}


	public FieldCreatorHelper dependingOnBuilder() {
		return this;
	}

	/**
	 * @param position the position to set
	 */
	public FieldCreatorHelper setPosition(int position) {
		this.position = position;
		
		return this;
	}

	/**
	 * @param length the length to set
	 */
	public FieldCreatorHelper setLength(int length) {
		this.length = length;
		
		return this;
	}

	/**
	 * @param childOccurs the childOccurs to set
	 */
	public FieldCreatorHelper setChildOccurs(int childOccurs) {
		this.childOccurs = childOccurs;
		
		return this;
	}


	/**
	 * @param foundRedefine the foundRedefine to set
	 */
	public void setFoundRedefine(boolean foundRedefine) {
		this.foundRedefine = foundRedefine;
	}
//
	/**
	 * 
	 * @param dependOnParentDtls
	 * @param dependingVar
	 * @return
	 */
	public DependingOn newDependingOn(IAddDependingOn currentLayout, DependingOnDtls dependOnParentDtls, String dependingVar) {
		DependingOn dependOn = null;
        if (dependingVar.length() > 0) {
        	//ExternalField tmpField = convertElement2Field("xx", false, basePosition, childElement, null);
        	String dependingVarNoIdx = dependingVar;
        	String key = dependingVar.toLowerCase();
        	String lookup = fieldToNameWithArrayIndexs.get(key);
        	if (lookup != null) {
        		dependingVar = lookup;
        		key = lookup.toLowerCase();
        	}
        	//System.out.println(dependingVar + "\t" + tmpField.getPos() + "\t" + length + "\t" + childOccurs);
    		dependOn = new DependingOn(dependingVar, dependingVarNoIdx, position, length, childOccurs);
        	if (dependOnParentDtls == null ) {
        		if (splitCopybook != ICobolSplitOptions.SPLIT_REDEFINE || foundRedefine) {
        			currentLayout.addDependingOn(dependOn);
         		} else {
         			if (commonDependingOn == null) {
         				commonDependingOn = new ArrayList<DependingOn>();
         			}
           			commonDependingOn.add(dependOn);
        		}
        	}
        }
        return dependOn;
	}

	/**
	 * @return the commonDependingOn
	 */
	public List<DependingOn> getCommonDependingOn() {
		return commonDependingOn;
	}

	/**
	 * Create a JRecord / RecordEditor Field-Name from a Cobol field name

	 * @return JRecord / RecordEditor Field-Name
	 */
	public String createFieldName(String cobolFieldName, String arrayIndexDetails) {

        String nameKey = cobolFieldName.toLowerCase();
        if (! "".equals(arrayIndexDetails)) {
            cobolFieldName += " (" + arrayIndexDetails + ")";
            fieldToNameWithArrayIndexs.put(nameKey, cobolFieldName);
        } else {
        	fieldToNameWithArrayIndexs.remove(nameKey);
        }

        if (dropCopybookFromFieldNames && cobolFieldName.toUpperCase().startsWith(copyBookPref)) {
            cobolFieldName = cobolFieldName.substring(copyBookPref.length());
        }
        return cobolFieldName;
	}
	
	/**
	 * Update the Original Field name if neccessary
	 * @param lName
	 * @return
	 */
	public String createOriginalName(String lName) {

        if (dropCopybookFromFieldNames && lName.toUpperCase().startsWith(copyBookPref)) {
             lName = lName.substring(copyBookPref.length());
        }
        return lName;
	}

	
	public String updateFieldNameIndex(String nameSuffix, int arrayIdx) {
        if (nameSuffix.equals("")) {
        	nameSuffix = Integer.toString(arrayIdx);
        } else {
        	nameSuffix = nameSuffix + ", " + arrayIdx;
        }
        return nameSuffix;
	}

	/**
	 * @return the dropCopybookFromFieldNames
	 */
	public boolean isDropCopybookFromFieldNames() {
		return dropCopybookFromFieldNames;
	}

	/**
	 * @param dropCopybookFromFieldNames the dropCopybookFromFieldNames to set
	 */
	public void setDropCopybookFromFieldNames(boolean dropCopybookFromFieldNames) {
		this.dropCopybookFromFieldNames = dropCopybookFromFieldNames;
	}



	public int deriveType(boolean isNumeric, String usage, String picture, boolean signSeperate, String signPosition, boolean justified) {
        int iType = Type.ftChar;
        picture = picture == null ? "" : picture;
        
        if (isNumeric) {
        	boolean signed = picture.length() > 0 && (picture.charAt(0) == 's' || picture.charAt(0) == 'S');
        	iType = numTranslator.getTypeIdentifier(
        								usage == null ? "" : usage, 
        								picture, signed,
        								signSeperate, signPosition);
        } else if (justified) {
        	iType = Type.ftCharRightJust;
        } else if ("null-padded".equals(usage)) {
        	iType = Type.ftCharNullPadded;
        } else if ("null-terminated".equals(usage)) {
        	iType = Type.ftCharNullTerminated;
        }

        return iType;
	}
	
    /**
     * Calculate the size of decimal portion
     * @param pFormat Cobol Picture
     * @return decimal size
     */
    public int calculateDecimalSize(int type, String pFormat, int scale) {
    	if (scale < 0 && scale != net.sf.cb2xml.def.IItem.NULL_INT_VALUE) { return scale; }
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
            
            if (lRet > 0 && (pFormat.endsWith("+") || pFormat.endsWith("-"))) {
            	lRet -= 1;
            }
        }

        return lRet;
    }
    
    
    public void updateGroup(int level, String lOrigName) {
    	
        if (level > 0 && level <= groupName.size()) {
     	   String s = groupName.get(level - 1);
     	   if (lOrigName != null && lOrigName.length() > 0 && ! "filler".equalsIgnoreCase(lOrigName)) {
     		   s = s + lOrigName + '.';
     	   } else {
     		   s = s + '.';
     	   }

     	   if (groupName.size() > level) {
     		   groupName.set(level, s);
     	   } else {
     		   groupName.add(s);
     	   }
        }
    }
    
    public int getGroupNameSize() {
    	return groupName.size();
    }
    
    public String getGroupName(int level) {
    	return groupName.get(level);
    }

	/**
	 * @return the copyBookPref
	 */
	public String getCopyBookPref() {
		return copyBookPref;
	}


	/**
	 * @return the font
	 */
	public String getFont() {
		return font;
	}

}
