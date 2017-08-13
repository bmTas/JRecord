package zRun;

import java.io.IOException;

import net.sf.JRecord.test.schema.DoCompare;
import net.sf.JRecord.test.schema.ParmDetails;

public class ZtstC2 {

	public static void main(String[] a) throws IOException {
		
		String[] args2 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "GNUCobol",
				ParmDetails.ARG_FILE_STRUCTURE, ParmDetails.MAINFRAME_VB.option,
				ParmDetails.ARG_FONT, "",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "Yes",
				ParmDetails.ARG_INPUT, "G:/Temp/LayoutsGnuCobolYes.txt.gz"
		};

		DoCompare.main(args2);
		
		String[] args3 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "GNUCobol",
				ParmDetails.ARG_FILE_STRUCTURE, ParmDetails.MAINFRAME_VB.option,
				ParmDetails.ARG_FONT, "",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "No",
				ParmDetails.ARG_INPUT, "G:/Temp/LayoutsGnuCobolNo.txt.gz"
		};

		DoCompare.main(args3);

	}

}
