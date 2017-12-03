package useCobolTree;

import java.io.IOException;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.detailsBasic.IItemDetails;
import net.sf.JRecord.detailsBasic.ItemDtl;

public class PrintInCobolTree {

	public void print(String copybook, String fileName) throws IOException {
		ICobolIOBuilder iob = JRecordInterface1.COBOL.newIOBuilder(copybook)
				.setFont("cp037")
				.setFileOrganization(Constants.IO_FIXED_LENGTH);
		LayoutDetail layout = iob.getLayout();
		AbstractLineReader reader = iob.newReader(fileName);
		AbstractLine l;
		
		try {
			while ((l = reader.read()) != null) {
				int recordNum = l.getPreferredLayoutIdx();
				print(layout.getRecord(recordNum).getCobolItems(), l, 0);
				System.out.println();
			}
		} finally {
			reader.close();
		}
	}
	
	private void print(List<? extends IItemDetails> tree, AbstractLine l, int lvl) {
		for (IItemDetails itm : tree) {
			if (! itm.getItemType().isArray) {
				StringBuilder b = new StringBuilder();
				
				for (int i = 0; i < lvl; i++) {
					b.append(' ').append(' ');
				}
				b.append(itm.getLevelString()).append(' ')
							.append(itm.getFieldName()).append(' ');
				if (itm.getItemType() == ItemDtl.ItemType.FIELD) {
					while (b.length() < 46) {
						b.append(' ');
					}
					b.setLength(45);
					b.append(' ').append(l.getFieldValue(itm.getFieldDefinition()).asString());
					System.out.println(b);
				} else {
					System.out.println(b);
					print(itm.getChildItems(), l, lvl + 1);
				}
				
			}
		}
	}
	
	public static void main(String[] args)  throws IOException  {
		String reDir = "/home/bruce/.RecordEditor/HSQLDB/";
		PrintInCobolTree pt = new PrintInCobolTree();
		
		pt.print(reDir + "CopyBook/Cobol/DTAR029.cbl", reDir + "/SampleFiles/DTAR020.bin");
	}

}
