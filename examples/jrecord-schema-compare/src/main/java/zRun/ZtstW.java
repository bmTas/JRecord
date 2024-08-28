package zRun;

import java.io.IOException;

import net.sf.JRecord.test.schema.ParmDetails;
import net.sf.JRecord.test.schema.WriteCopybookCompareFile;

public class ZtstW {

	public static void main(String[] a) throws IOException {
		String[] args1 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "Mainframe",
				ParmDetails.ARG_FILE_STRUCTURE, ParmDetails.MAINFRAME_VB.option,
				ParmDetails.ARG_FONT, "cp037",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "No",
				ParmDetails.ARG_OUTPUT, "G:/Temp/LayoutsMainframeNo.txt.gz"
		};

		String[] args1a = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "Mainframe",
				ParmDetails.ARG_FILE_STRUCTURE, "Default",
				ParmDetails.ARG_FONT, "cp037",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "No",
				ParmDetails.ARG_OUTPUT, "G:/Temp/LayoutsMainframeNoDefault.txt.gz"
		};

		WriteCopybookCompareFile.main(args1);
		WriteCopybookCompareFile.main(args1a);
		
		String[] args2 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "GNUCobol",
				ParmDetails.ARG_FILE_STRUCTURE, ParmDetails.MAINFRAME_VB.option,
				ParmDetails.ARG_FONT, "",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "Yes",
				ParmDetails.ARG_OUTPUT, "G:/Temp/LayoutsGnuCobolYes.txt.gz"
		};

		WriteCopybookCompareFile.main(args2);
		
		String[] args3 = {
				ParmDetails.ARG_DIRECTORY, "F:/Work/EclipseWorkspaces/std_workspace/z_Cb2xml_tests/src/common/cobolCopybook",
				ParmDetails.ARG_DIALECT, "GNUCobol",
				ParmDetails.ARG_FILE_STRUCTURE, ParmDetails.MAINFRAME_VB.option,
				ParmDetails.ARG_FONT, "",
				ParmDetails.ARG_DROP_COPBOOK_NAME, "No",
				ParmDetails.ARG_OUTPUT, "G:/Temp/LayoutsGnuCobolNo.txt.gz"
		};

		WriteCopybookCompareFile.main(args3);

	}

}
