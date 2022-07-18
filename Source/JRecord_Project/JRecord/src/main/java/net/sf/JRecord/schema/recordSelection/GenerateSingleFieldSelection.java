/**
 * 
 */
package net.sf.JRecord.schema.recordSelection;

import java.util.List;

import net.sf.JRecord.cgen.def.gen.ICodeGenDefinition;
import net.sf.JRecord.def.recordSelection.IRecordSelectionDetails;
import net.sf.JRecord.def.recordSelection.ISingleFieldDeciderDetails;

/**
 * @author bruce
 *
 */
public class GenerateSingleFieldSelection implements ICodeGenDefinition {

	private final ISingleFieldDeciderDetails deciderDetails;
	/**
	 * 
	 */
	public GenerateSingleFieldSelection(ISingleFieldDeciderDetails deciderDetails) {
		this.deciderDetails = deciderDetails;
	}
	
	

	@Override
	public String importClass() {
		return "";// "net.sf.JRecord.JRecordInterface1";
	}



	@Override
	public String createClass() {
		StringBuilder b = new StringBuilder();
		List<IRecordSelectionDetails> recordSelectionDetails = deciderDetails.getRecordSelectionDetails();
		
		b.append("JRecordInterface1.RECORD_DECIDER_BUILDER\n\t\t\t\t.singleFieldDeciderBuilder(\"")
		 .append( deciderDetails.getRecordTypeFieldName())
		 .append("\", ");
		if (deciderDetails.getDefaultRecordName() == null || deciderDetails.getDefaultRecordName().length() == 0) {
			b
			 .append(deciderDetails.isAllowOtherRecordTypes())
			 .append(")\n");
		} else {
			b
			 .append("\"").append(deciderDetails.getDefaultRecordName())
			 .append("\")\n");
		}
		for (IRecordSelectionDetails recDtls : recordSelectionDetails) {
			b.append("\t\t\t\t\t.addRecord(\"")
			 .append(recDtls.getRecordTypeValue()) .append("\", \"")
			 .append(recDtls.getRecordName())   .append("\")\n");
		}
		b.append("\t\t\t\t.build()");
		return b.toString();
	}

}
