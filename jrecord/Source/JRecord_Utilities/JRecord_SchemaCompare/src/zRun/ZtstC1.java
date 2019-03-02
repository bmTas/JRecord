package zRun;

import java.io.IOException;

import net.sf.JRecord.test.schema.DoCompare;
import net.sf.JRecord.test.schema.ParmDetails;

public class ZtstC1 {

	public static void main(String[] a) throws IOException {
		String[] args1 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "Mainframe",
				ParmDetails.ARG_FILE_STRUCTURE, ParmDetails.MAINFRAME_VB.option,
				ParmDetails.ARG_FONT, "cp037",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "No",
				ParmDetails.ARG_INPUT, "G:/Temp/LayoutsMainframeNo.txt.gz"
		};

		DoCompare.main(args1);
		
		String[] args2 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "Mainframe",
				ParmDetails.ARG_FILE_STRUCTURE, "Default",
				ParmDetails.ARG_FONT, "cp037",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "No",
				ParmDetails.ARG_INPUT, "G:/Temp/LayoutsMainframeNoDefault.txt.gz"
		};

		DoCompare.main(args2);

	}

}
