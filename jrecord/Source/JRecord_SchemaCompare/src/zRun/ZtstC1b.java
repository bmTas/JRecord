package zRun;

import java.io.IOException;

import net.sf.JRecord.test.schema.DoCompare;
import net.sf.JRecord.test.schema.ParmDetails;

public class ZtstC1b {

	public static void main(String[] a) throws IOException {
		// Compare File generated for GNU Cobol with Mainframe Layouts
		// This should generate some differeces
		String[] args1 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "Mainframe",
				ParmDetails.ARG_FILE_STRUCTURE, ParmDetails.MAINFRAME_VB.option,
				ParmDetails.ARG_FONT, "cp037",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "No",
				ParmDetails.ARG_INPUT, "G:/Temp/LayoutsMainframeNoDefault.txt.gz"
		};

		DoCompare.main(args1);
//		
//		String[] args2 = {
//				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
//				ParmDetails.ARG_DIALECT, "GNUCobol",
//				ParmDetails.ARG_FONT, "",
//				ParmDetails.ARG_DROP_COPBOOK_NAME, "Yes",
//				ParmDetails.ARG_OUTPUT, "G:/Temp/LayoutsGnuCobolYes.txt.gz"
//		};
//
//		WriteCopybookCompareFile.main(args2);

	}

}
