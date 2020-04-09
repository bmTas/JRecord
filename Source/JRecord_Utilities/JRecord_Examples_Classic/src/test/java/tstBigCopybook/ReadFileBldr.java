package tstBigCopybook;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.def.IItem;

public class ReadFileBldr {

	
//	private final String fname;
	
	ReadFileBldr(String fname) throws FileNotFoundException {

		List<? extends IItem> itemTree = Cb2Xml3.newBuilder(new File(fname))
												.asCobolItemTree()
													.getChildItems();
	}
	
	
	public static void main(String[] args) throws FileNotFoundException {
		new ReadFileBldr(args == null || args.length == 0 ? "/media/sf_Shared/LargeCopybook.cbl" : args[0]);
	}

}
