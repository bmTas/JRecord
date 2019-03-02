package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

/**
 * This class tests Occurs Depending when 'Size' field is in a 
 * normal Array
 * 
 * @author Bruce Martin
 *
 */
public class TstOccursDepending61 extends TestCase {
	private static String[] LINES = {
			"line 0     0>1 00000 1>2 00010 00011 2>3 00020 00021 00022 3>4 00030 00031 00032 00033",
			"line 1     0>2 00100 00101 1>3 00110 00111 00112 2>4 00120 00121 00122 00123 3>5 00130 00131 00132 00133 00134",
			"line 2     0>3 00200 00201 00202 1>4 00210 00211 00212 00213 2>5 00220 00221 00222 00223 00224 3>1 00230",
			"line 3     0>4 00300 00301 00302 00303 1>5 00310 00311 00312 00313 00314 2>1 00320 3>2 00330 00331",
			"line 4     0>5 00400 00401 00402 00403 00404 1>1 00410 2>2 00420 00421 3>3 00430 00431 00432",
			"line 5     0>1 00500 1>2 00510 00511 2>3 00520 00521 00522 3>4 00530 00531 00532 00533",
			"line 6     0>2 00600 00601 1>3 00610 00611 00612 2>4 00620 00621 00622 00623 3>5 00630 00631 00632 00633 00634",
			"line 7     0>3 00700 00701 00702 1>4 00710 00711 00712 00713 2>5 00720 00721 00722 00723 00724 3>1 00730",
			"line 8     0>4 00800 00801 00802 00803 1>5 00810 00811 00812 00813 00814 2>1 00820 3>2 00830 00831",
			"line 9     0>5 00900 00901 00902 00903 00904 1>1 00910 2>2 00920 00921 3>3 00930 00931 00932",
			"line 10    0>1 01000 1>2 01010 01011 2>3 01020 01021 01022 3>4 01030 01031 01032 01033",
			"line 11    0>2 01100 01101 1>3 01110 01111 01112 2>4 01120 01121 01122 01123 3>5 01130 01131 01132 01133 01134",
			"line 12    0>3 01200 01201 01202 1>4 01210 01211 01212 01213 2>5 01220 01221 01222 01223 01224 3>1 01230",
			"line 13    0>4 01300 01301 01302 01303 1>5 01310 01311 01312 01313 01314 2>1 01320 3>2 01330 01331",
			"line 14    0>5 01400 01401 01402 01403 01404 1>1 01410 2>2 01420 01421 3>3 01430 01431 01432",
			"line 15    0>1 01500 1>2 01510 01511 2>3 01520 01521 01522 3>4 01530 01531 01532 01533",
			"line 16    0>2 01600 01601 1>3 01610 01611 01612 2>4 01620 01621 01622 01623 3>5 01630 01631 01632 01633 01634",
			"line 17    0>3 01700 01701 01702 1>4 01710 01711 01712 01713 2>5 01720 01721 01722 01723 01724 3>1 01730",
			"line 18    0>4 01800 01801 01802 01803 1>5 01810 01811 01812 01813 01814 2>1 01820 3>2 01830 01831",
			"line 19    0>5 01900 01901 01902 01903 01904 1>1 01910 2>2 01920 01921 3>3 01930 01931 01932",
			"line 20    0>1 02000 1>2 02010 02011 2>3 02020 02021 02022 3>4 02030 02031 02032 02033",
			"line 21    0>2 02100 02101 1>3 02110 02111 02112 2>4 02120 02121 02122 02123 3>5 02130 02131 02132 02133 02134",
			"line 22    0>3 02200 02201 02202 1>4 02210 02211 02212 02213 2>5 02220 02221 02222 02223 02224 3>1 02230",
			"line 23    0>4 02300 02301 02302 02303 1>5 02310 02311 02312 02313 02314 2>1 02320 3>2 02330 02331",
			"line 24    0>5 02400 02401 02402 02403 02404 1>1 02410 2>2 02420 02421 3>3 02430 02431 02432",
			"line 25    0>1 02500 1>2 02510 02511 2>3 02520 02521 02522 3>4 02530 02531 02532 02533",
			"line 26    0>2 02600 02601 1>3 02610 02611 02612 2>4 02620 02621 02622 02623 3>5 02630 02631 02632 02633 02634",
			"line 27    0>3 02700 02701 02702 1>4 02710 02711 02712 02713 2>5 02720 02721 02722 02723 02724 3>1 02730",
			"line 28    0>4 02800 02801 02802 02803 1>5 02810 02811 02812 02813 02814 2>1 02820 3>2 02830 02831",
			"line 29    0>5 02900 02901 02902 02903 02904 1>1 02910 2>2 02920 02921 3>3 02930 02931 02932"
	};
	
	public void testUpdate() throws IOException {
		String copybookFileName = WriteOD61.class.getResource("OccursDependingOn61.cbl").getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME);
		AbstractLine line;
		
		for (int i = 0; i < 30; i++) {
			line = ioBuilder.newLine();
			line.getFieldValue("Test-id").set("line " + i);
			
			for (int j = 0; j < 4; j++) {
				int count = (i + j) % 5 + 1;
				line.getFieldValue("text (" + j + ")").set(" " + j + ">");
				line.getFieldValue("week-of-month (" + j + ")").set(count);
				
				for (int k = 0; k < count; k++) {
					String index = " (" + j + ", " + k + ")";
					line.getFieldValue("sep" + index)          .set(" ");
					line.getFieldValue("daily-sales" + index)  .set(i * 100 + j * 10 + k);
				}
			}
			assertEquals(LINES[i], line.getFullLine());
		}
	}
	

	public void testCheck() throws IOException {
		String copybookFileName = WriteOD61.class.getResource("OccursDependingOn61.cbl").getFile();
		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME);
		AbstractLine line;
		
		for (int i = 0; i < 30; i++) {
			line = ioBuilder.newLine();
			line.setData(LINES[i]);
			
			assertEquals("line " + i, line.getFieldValue("Test-id").asString());
			
			for (int j = 0; j < 4; j++) {
				int count = (i + j) % 5 + 1;
				assertEquals(" " + j + ">", line.getFieldValue("text (" + j + ")").asString());
				assertEquals(count, line.getFieldValue("week-of-month (" + j + ")").asInt());
				
				for (int k = 0; k < count; k++) {
					String index = " (" + j + ", " + k + ")";
					assertEquals("", line.getFieldValue("sep" + index)          .asString());
					assertEquals(i * 100 + j * 10 + k, line.getFieldValue("daily-sales" + index)  .asInt());
				}
			}
		}
	}

}
