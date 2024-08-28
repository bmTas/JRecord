package net.sf.JRecord.x.cobol.copyReplacing;

import java.io.StringReader;

public class Tst {

	private static String copybookStr
			= "000100  01    :L2BS:-RECORD.\n"
			+ "000200            03    :L2BS:-KEY.\n"
			+ "000300                      05    :L2BS:-KEY-ORG-X.\n"
			+ "000400                                07    :L2BS:-KEY-ORG    PIC 999.\n"
			+ "000500                      05    :L2BS:-KEY-ACCT             PIC X(19).\n"
			+ "000600*\n"
			+ "000700            03    :L2BS:-GROUP-HIGH-1.\n"
			+ "000800                      05    :L2BS:-GROUP-HIGH-1-1.\n"
			+ "000900                                07    :L2BS:-LOGO       PIC 999.\n"
			+ "001000                                07    :L2BS:-SHORT-NAME    \n"
			+ "001100                                                        PIC X(20).\n"
			+ "001200                                07    :L2BS:-CUST-NBR   PIC X(19).\n";

	public static void main(String[] args) {
		System.out.println(
				CopyReplacingBldr.newBldr(new StringReader(copybookStr), CobolColumnDetails.STANDARD_COLUMNS)
					.replacing(":L2BS:", "LBS")
					.asString()
		);
	}

}
