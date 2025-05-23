package net.sf.JRecord.External.base;

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import lombok.Setter;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.Cb2xmlJrConsts;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.cb2xml.IReadCopybook;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.Convert;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.cb2xml.analysis.BaseItem;
import net.sf.cb2xml.analysis.Copybook;
import net.sf.cb2xml.analysis.Item;
import net.sf.cb2xml.def.ICopybookJrUpd;
import net.sf.cb2xml.def.IItem;
import net.sf.cb2xml.def.IItemBase;
import net.sf.cb2xml.def.IItemJrUpd;

public class BaseCobolItemLoader<XRecord extends BaseExternalRecord<XRecord>> implements Cloneable {


    private static final String STR_YES = "Y";
    private static final String STR_NO  = "N";    

    private final boolean useJRecordNaming ;
    private final IExernalRecordBuilder<XRecord> recBuilder;
    private final IReadCopybook readCopybook;

    @Setter
    private int stackSize = Cb2xmlJrConsts.CALCULATE_THREAD_SIZE;

    @Setter
    private boolean keepFiller, dropCopybookFromFieldNames = CommonBits.isDropCopybookFromFieldNames(), saveCb2xml;
    
    
	public BaseCobolItemLoader(boolean useJRecordNaming, 
			IExernalRecordBuilder<XRecord> recBuilder,
			IReadCopybook readCopybook) {
		super();
		this.useJRecordNaming = useJRecordNaming;
		this.recBuilder = recBuilder;
		this.readCopybook = readCopybook;
	}
	
	
	public void setKeepFillers(boolean keepFiller) {
		this.keepFiller = keepFiller;
	}

    public void setSaveCb2xmlDocument(boolean saveCb2xml) {
		this.saveCb2xml = saveCb2xml;
	}


	/**
	 * @return the stackSize
	 */
	protected int getStackSize() {
		return stackSize;
	}


    /* (non-Javadoc)
	 * @see net.sf.JRecord.External.CopybookLoader#loadCopyBook(java.lang.String, int, int, java.lang.String, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	public final XRecord loadCopyBook(String copyBookFile,
			int splitCopybookOption, int dbIdx, String font, int binFormat,
			int systemId, AbsSSLogger log) throws IOException {
		
		return loadCopyBook(new FileReader(copyBookFile), Conversion.getCopyBookId(copyBookFile),
				splitCopybookOption, dbIdx,
				font, CommonBits.getDefaultCobolTextFormat(), binFormat, systemId, log);
		
	}
	

    /* (non-Javadoc)
	 * @see net.sf.JRecord.External.ICopybookLoaderStream#loadCopyBook(java.io.InputStream, java.lang.String, int, int, java.lang.String, int, int, int, net.sf.JRecord.Log.AbsSSLogger)
	 */
	public XRecord loadCopyBook(InputStream inputStream,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		return loadCopyBook(new InputStreamReader(inputStream), copyBookName, splitCopybook, dbIdx, font, copybookFormat, binaryFormat, systemId, log);
	}


	/**
     * Convert a XML Dom Copybook into the XRecord
     *
     * @param copyBookFile Copy Book file Name
     * @param splitCopybookOption whether to split a copy book on a redefine
     * @param dbIdx Database Index
     * @param font font name to use
     * @param binFormat binary format to use
     * @param systemId System Identifier
     * @param log log where any messages should be written
     *
     * @return record to be inserted
     *
     * @throws IOException error to be handled by calling program
     */
    public XRecord loadCopyBook(final String copyBookFile,
            					final int splitCopybookOption,
            					final int dbIdx,
            					final String font,
            					final int copybookFormat,
            					final int binFormat,
            					final int systemId,
            					final AbsSSLogger log)
    		throws IOException {

    	return loadCopyBook(new FileReader(copyBookFile), Conversion.getCopyBookId(copyBookFile),
    			splitCopybookOption, dbIdx, font, copybookFormat, binFormat, systemId, log);
    }


	

	public XRecord loadCopyBook(Reader reader,
			String copyBookName, int splitCopybook, int dbIdx, String font,
			int copybookFormat, int binaryFormat, int systemId, AbsSSLogger log)
			throws IOException {
		
		Copybook copybook = readCopybook.getCopybook(reader, copyBookName, binaryFormat, false, copybookFormat, stackSize);
		return loadCopybook(copybook, copyBookName, splitCopybook, dbIdx, font, binaryFormat, systemId);
	}

	/**
	 * @param copybook
	 * @param copyBookName
	 * @param splitCopybook
	 * @param dbIdx
	 * @param font
	 * @param binaryFormat
	 * @param systemId
	 * @return
	 */
	public XRecord loadCopybook(ICopybookJrUpd copybook, String copyBookName, int splitCopybook, int dbIdx, String font,
			int binaryFormat, int systemId) {
		XRecord ret;
       	List<? extends IItemJrUpd> copybookItems = copybook.getChildItems();
       	
       	allocDBs(dbIdx);
       	
        switch (splitCopybook) {
        case ICobolSplitOptions.SPLIT_NONE:   /* split copybook on first redefine*/
        	ret = createSingleRecordSchema(copyBookName, null, font, binaryFormat, copybookItems);
            break;
        case ICobolSplitOptions.SPLIT_REDEFINE:
        	ret = createSplitOnRedefinesSchema(copyBookName, font, binaryFormat, systemId, copybookItems);
            break;
        case ICobolSplitOptions.SPLIT_TOP_LEVEL:
           	ret = createGroupRecordSchema(copyBookName, null, font, binaryFormat, systemId, copybookItems, true);
            break;
        case ICobolSplitOptions.SPLIT_HIGHEST_REPEATING:
        	ArrayList<String> groups = new ArrayList<String>();
        	List<? extends IItemJrUpd> list = copybookItems;
        	while (list != null && list.size() == 1) {
        		IItemJrUpd itm = list.get(0);
        		String fn = itm.getFieldName();
        		if (fn != null && !fn.isEmpty() && ! "filler".equalsIgnoreCase(fn)) {
        			groups.add(fn);
        		}
				list = itm.getChildItems();
        	}
           	String[] groupArray = groups.toArray(new String[0]);
        	if (list == null || list.size() < 2) {
            	ret = createSingleRecordSchema(copyBookName, groupArray, font, binaryFormat, copybookItems);
        	} else {
				ret = createGroupRecordSchema(
               			copyBookName, groupArray, 
               			font, binaryFormat, systemId, list, true);
        	}
            break;
        default:
        	ret = createGroupRecordSchema(copyBookName, null, font, binaryFormat, systemId, copybookItems, false);
        }
        
        
        ret.setCopybook(copybook);
        updateRecord(copyBookName, systemId, font, ret, STR_YES);
        
        Convert numTranslator = ConversionManager.getInstance().getConverter4code(binaryFormat) ;
        boolean multipleRecordLengths = false, binary = false;

        if (ret.getNumberOfRecords() == 0) {
            binary = ret.isBinary();
        } else {
            int len = getRecLength(ret.getRecord(0));
            binary = ret.getRecord(0).isBinary();

            for (int i = 1; i < ret.getNumberOfRecords(); i++) {
                binary = binary || ret.getRecord(i).isBinary();
                multipleRecordLengths = multipleRecordLengths
                                     || (len != getRecLength(ret.getRecord(i)));
            }
        }
        ret.setFileStructure(numTranslator.getFileStructure(multipleRecordLengths, binary));
        ret.setSplitOption(splitCopybook);

        freeDBs(dbIdx);       

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


	private XRecord createSingleRecordSchema(String copyBookName, String[] groupArray, String font, int binaryFormat,
			List<? extends IItemJrUpd> copybookItems) {
		XRecord ret = createRecord(copyBookName, font, binaryFormat);
		ret.setItems(copyBookName, groupArray, binaryFormat, copybookItems);
		ret.updateTypeOnCobolItems();
		return ret;
	}

	/**
	 * Create a `Group` record
	 */
	private XRecord createGroupRecordSchema(String copyBookName, String[] parentGroupNames, String font, int binaryFormat, int systemId,
			List<? extends IItemJrUpd> copybookItems, boolean updatePosition) {

		IItemJrUpd itm;
		XRecord childRec;
		XRecord ret = createGroupRecord(copyBookName, font, binaryFormat);

		for (int i = 0; i < copybookItems.size(); i++) {
			itm = copybookItems.get(i);
			childRec = createChildRecord(copyBookName, itm.getFieldName(), i, font, binaryFormat, systemId);
			
			if (updatePosition) {
				itm.updatePosition(- itm.getPosition() + 1);
			}

			childRec.setItems(copyBookName, parentGroupNames, binaryFormat, itm);
			childRec.updateTypeOnCobolItems();
			ret.addRecord(childRec);
		}
		return ret;
	}
	
	private XRecord createSplitOnRedefinesSchema(String copyBookName, String font, int binaryFormat, int systemId,
			List<? extends IItemJrUpd> copybookItems) {
		
		XRecord childRec;
		XRecord ret = createGroupRecord(copyBookName, font, binaryFormat);
		RedefineSearcher redef = new RedefineSearcher(copybookItems);
		
		if (redef.item == null) {
			return createSingleRecordSchema(copyBookName, null, font, binaryFormat, copybookItems);
		}
		
		int idx = 0;
		for (IItem redefItem : redef.redefineItems) {
			childRec = createChildRecord(copyBookName, redefItem.getFieldName(), idx++, font, binaryFormat, systemId);
			childRec.setItems(
					copyBookName,
					null,
					binaryFormat,
					createSplitOnRedefines_processList(
								redefItem, new Copybook("", ""),
								redef, copybookItems)
					);
			childRec.updateTypeOnCobolItems();
			ret.addRecord(childRec);
		}
		
		return ret;
	}

	private List<? extends IItemJrUpd> createSplitOnRedefines_processList(
			IItem redefItem, BaseItem parent, 
			RedefineSearcher redef, List<? extends IItemJrUpd> list) {
		if (list == null || list.size() == 0) { return list; }
		
		return (new RedefineCopy(redefItem, redef, list)).createSplit();
	}
		

//	String indent = "\t";
//	private List<? extends IItemJrUpd> createSplitOnRedefines_processList(
//			IItem redefItem, BaseItem parent, 
//			RedefineSearcher redef, List<? extends IItemJrUpd> list) {
//		if (list == null || list.size() == 0) { return list; }
//		String oindent = indent;
//		boolean foundRedefine = false;
//		boolean topLevel = true;
//		boolean addTo = true;// ! "\t".equals(oindent);
//		indent += "\t";
//		System.out.println(indent + "||| " + redefItem.getFieldName() + " " 
//				+ (parent instanceof Item ? ((Item) parent).getFieldName() : parent.toString())) ;
//		List<IItemJrUpd> nList = new ArrayList<IItemJrUpd>(list.size());
//		for (IItemJrUpd itm : list) {
//			if (itm == redefItem) {
//				nList.add(itm);
//				foundRedefine = true;
//				System.out.println(indent + "== 1 ==> " + itm.getFieldName());
//			} else if (redef.parents.contains(itm)) {
//				Item newItem = new Item(parent, itm.getLevelNumber(), itm.getLevelString(), itm.getFieldName());
//				newItem.set(itm);
//				boolean add = itm == redef.item;
////				for (IItemJrUpd ci : itm.getChildItems()) {
////					if (ci == redef.item) {
////						add = false;
////						break;
////					}
////				}
//				List<? extends IItemJrUpd> children = createSplitOnRedefines_processList(redefItem, newItem, redef, itm.getChildItems());
////				for (IItemJrUpd ci : children) {
////					//if (ci.getChildItems().size() == 0) {
////						newItem.addItem((Item)ci);
////						System.out.println(indent + "  == a ==> " + newItem.getFieldName() + " " + ci.getFieldName() + " " + ci.getChildItems().size() + add);
////					//}
////				}
//				nList.add(newItem);
//				System.out.println(indent + "== 2 ==> " + itm.getFieldName() + " " + (redef.item == itm));
//			} else if (! redef.redefineItems.contains(itm)) {
//				if (topLevel) {
//					nList.add(itm);
//				}
//				System.out.println(indent + "== 3 ==> " + itm.getFieldName());
//			}
//		}
//
//		if (foundRedefine) {
//			for (IItemJrUpd ci : nList) {
//				parent.addItem((Item)ci);
//				System.out.println(indent + "  == z ==> " + (parent instanceof Item ? ((Item) parent).getFieldName() : "~~" ) + " "
//						+ ci.getFieldName() + " " + ci.getChildItems().size());
//			}
//		}
//		indent = oindent;
//		
//		return nList;
//	}
	

	private XRecord createChildRecord(String copyBookName, String fieldName, int idx, String font, int binaryFormat,
			int systemId) {
		XRecord childRec;
		String name = fieldName; 
		if (name == null || name.isEmpty() || "filler".equalsIgnoreCase(name)) {
			name = copyBookName + "_" + idx;
		} else 			if (useJRecordNaming) {
			name = name.trim();
    	} else {
      	   name = copyBookName.trim() + "-" + name.trim();
		}

		childRec = createRecord(name, font, binaryFormat);
		updateRecord(name, systemId, font, childRec, STR_NO);
		return childRec;
	}

	@SuppressWarnings("deprecation")
	protected void updateRecord(String copyBookName, int systemId, String font, XRecord ret, String list) {
		ret.setListChar(list);
        ret.setSystem(systemId);
        ret.setCopyBook(copyBookName);
        ret.setNew(true);
	}   

	private XRecord createRecord(String copyBookName, String font, int binaryFormat) {
        int rt = Constants.rtRecordLayout;
        if (binaryFormat == ICopybookDialects.FMT_MAINFRAME) {
            rt = Constants.rtBinaryRecord;
        }

       return  recBuilder.getNullRecord(
    		   					copyBookName,
				   				rt,
				   				font)
    		   			.setCobolConversionOptions(keepFiller, dropCopybookFromFieldNames, saveCb2xml, useJRecordNaming);
	}


	private XRecord createGroupRecord(String copyBookName, String font, int binaryFormat) {
        int rt = Constants.rtGroupOfRecords;
        if (binaryFormat == ICopybookDialects.FMT_MAINFRAME
        ||  binaryFormat == ICopybookDialects.FMT_BIG_ENDIAN) {
            rt = Constants.rtGroupOfBinaryRecords;
        }

       return  recBuilder.getNullRecord(
    		   					copyBookName,
				   				rt,
				   				font)
	   					.setCobolConversionOptions(keepFiller, dropCopybookFromFieldNames, saveCb2xml, useJRecordNaming);
	}


	private static class RedefineCopy {
		final IItem redefItem;
		//BaseItem parent, 
		final RedefineSearcher redef;
		final List<? extends IItemJrUpd> list;
//		boolean searchingForSplit = true;
//		Set<IItem> redefItemSet = new HashSet<>();
		String indent = "";
		
		public RedefineCopy(IItem redefItem, RedefineSearcher redef, List<? extends IItemJrUpd> list) {
			super();
			this.redefItem = redefItem;
			this.redef = redef;
			this.list = list;
//			this.redefItemSet.addAll(redef.redefineItems);
		}
		
		List<? extends IItemJrUpd> createSplit() {
			if (list == null || list.size() == 0) { return list; }
			List<IItemJrUpd> nList = doSplit(new Copybook("", ""), list);
			
			return nList;
		}

		protected List<IItemJrUpd> doSplit(BaseItem parent, List<? extends IItemJrUpd> list) {
			String oindent = indent;
			indent += "\t";
			List<IItemJrUpd> nList = new ArrayList<IItemJrUpd>(list.size());
			for (IItemJrUpd itm : list) {
				//System.out.print(indent + "** " + itm.getFieldName());
				if (itm == redefItem) {
					//System.out.println(indent + " 1 > " + itm.getFieldName());
					nList.add(copyItem(parent, itm));
//					searchingForSplit = false;
				} else if (redef.parents.contains(itm)) {
					//System.out.println(indent + " 2 > " + itm.getFieldName());
					Item newItem = new Item(parent, itm.getLevelNumber(), itm.getLevelString(), itm.getFieldName());
					newItem.set(itm);
					doSplit(newItem, itm.getChildItems());
					nList.add(newItem);
				} else if (! redef.redefineItems.contains(itm)) {
					//System.out.println(indent + " 3 > " + itm.getFieldName());
					nList.add(copyItem(parent, itm));
				} else {
					System.out.println(indent + " --> " + itm.getFieldName());
				}
				//System.out.println();
			}
			

			indent = oindent;
			return nList;
		}
		
		private Item copyItem(BaseItem parent, IItemJrUpd itm) {
			Item newItem = new Item(parent, itm.getLevelNumber(), itm.getLevelString(), itm.getFieldName());
			newItem.set(itm);
			List<? extends IItemJrUpd> childItems = itm.getChildItems();
			for (IItemJrUpd child : childItems) {
				copyItem(newItem, child);
			}
			
			return newItem;
		}

	}
	/**
	 * Class to search for Redefines and
	 * store retrieved data in a useful for
	 * later processing
	 * 
	 * @author Bruce Martin
	 *
	 */
	private static class RedefineSearcher {
		int level = Integer.MAX_VALUE;
		IItemJrUpd item;
		List<IItem> redefineItems;
		Set<IItemBase> parents;
		
		RedefineSearcher(List<? extends IItemJrUpd> items) {
			search(items);
			
			if (item != null) {
				redefineItems = new ArrayList<>();
				int pos = item.getPosition();
				for (IItem itm : item.getParent().getChildItems()) {
					if (itm.getPosition() == pos) {
						redefineItems.add(itm);
					} 
				}
				
				parents = new HashSet<>();
				
				IItemBase p = item;
				while (p.getParent()!= null) {
					p = p.getParent();
					parents.add(p);
				}
			}
		}
		
		private void search(List<? extends IItemJrUpd> items) {
			if (items == null || items.size() == 0 || items.get(0).getLevelNumber() >= level) { return ;}
			
			for (IItemJrUpd itm : items) {
				if (itm.getLevelNumber() < level && (itm.isFieldRedefined() || itm.isFieldRedefines())) {
					level = itm.getLevelNumber();
					item = itm;
					return;
				}
			}
			
			for (IItemJrUpd itm : items) {
				search(itm.getChildItems());
			}
		}
	}
}
