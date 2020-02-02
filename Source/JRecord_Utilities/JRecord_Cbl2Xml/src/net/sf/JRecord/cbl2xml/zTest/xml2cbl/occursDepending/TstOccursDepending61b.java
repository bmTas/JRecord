package net.sf.JRecord.cbl2xml.zTest.xml2cbl.occursDepending;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;


import javax.xml.stream.XMLStreamException;

import junit.framework.TestCase;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.cbl2xml.Cobol2Xml;
import net.sf.JRecord.cbl2xml.def.ICobol2Xml;
import net.sf.JRecord.cbl2xml.zTest.xml2cbl.TstXmlConstants;


/**
 * This class tests Occurs Depending when 'Size' field is in a 
 * normal Array
 * 
 * @author Bruce Martin
 *
 */
public class TstOccursDepending61b extends TestCase {
//	private static String[] LINES = {
//			"line 0     0>1 00000 1>2 00010 00011 2>3 00020 00021 00022 3>4 00030 00031 00032 00033",
//			"line 1     0>2 00100 00101 1>3 00110 00111 00112 2>4 00120 00121 00122 00123 3>5 00130 00131 00132 00133 00134",
//			"line 2     0>3 00200 00201 00202 1>4 00210 00211 00212 00213 2>5 00220 00221 00222 00223 00224 3>1 00230",
//			"line 3     0>4 00300 00301 00302 00303 1>5 00310 00311 00312 00313 00314 2>1 00320 3>2 00330 00331",
//			"line 4     0>5 00400 00401 00402 00403 00404 1>1 00410 2>2 00420 00421 3>3 00430 00431 00432",
//			"line 5     0>1 00500 1>2 00510 00511 2>3 00520 00521 00522 3>4 00530 00531 00532 00533",
//			"line 6     0>2 00600 00601 1>3 00610 00611 00612 2>4 00620 00621 00622 00623 3>5 00630 00631 00632 00633 00634",
//			"line 7     0>3 00700 00701 00702 1>4 00710 00711 00712 00713 2>5 00720 00721 00722 00723 00724 3>1 00730",
//			"line 8     0>4 00800 00801 00802 00803 1>5 00810 00811 00812 00813 00814 2>1 00820 3>2 00830 00831",
//			"line 9     0>5 00900 00901 00902 00903 00904 1>1 00910 2>2 00920 00921 3>3 00930 00931 00932",
//	};
	private static String[] LINES2 = {
		"line 1   0",
		"line 2   1 0>1 00000",
		"line 3   1 0>2 00100 00101",
		"line 4   1 0>3 00200 00201 00202",
		"line 5   1 0>4 00300 00301 00302 00303",
		"line 6   1 0>5 00400 00401 00402 00403 00404",
		"line 7   1 0>1 00500",
		"line 8   1 0>2 00600 00601",
		"line 9   1 0>3 00700 00701 00702",
		"line 10  1 0>4 00800 00801 00802 00803",
		"line 11  1 0>5 00900 00901 00902 00903 00904",
		"line 12  2 0>1 00000 1>2 00010 00011",
		"line 13  2 0>2 00100 00101 1>3 00110 00111 00112",
		"line 14  2 0>3 00200 00201 00202 1>4 00210 00211 00212 00213",
		"line 15  2 0>4 00300 00301 00302 00303 1>5 00310 00311 00312 00313 00314",
		"line 16  2 0>5 00400 00401 00402 00403 00404 1>1 00410",
		"line 17  2 0>1 00500 1>2 00510 00511",
		"line 18  2 0>2 00600 00601 1>3 00610 00611 00612",
		"line 19  2 0>3 00700 00701 00702 1>4 00710 00711 00712 00713",
		"line 20  2 0>4 00800 00801 00802 00803 1>5 00810 00811 00812 00813 00814",
		"line 21  2 0>5 00900 00901 00902 00903 00904 1>1 00910",
		"line 22  3 0>1 00000 1>2 00010 00011 2>3 00020 00021 00022",
		"line 23  3 0>2 00100 00101 1>3 00110 00111 00112 2>4 00120 00121 00122 00123",
		"line 24  3 0>3 00200 00201 00202 1>4 00210 00211 00212 00213 2>5 00220 00221 00222 00223 00224",
		"line 25  3 0>4 00300 00301 00302 00303 1>5 00310 00311 00312 00313 00314 2>1 00320",
		"line 26  3 0>5 00400 00401 00402 00403 00404 1>1 00410 2>2 00420 00421",
		"line 27  3 0>1 00500 1>2 00510 00511 2>3 00520 00521 00522",
		"line 28  3 0>2 00600 00601 1>3 00610 00611 00612 2>4 00620 00621 00622 00623",
		"line 29  3 0>3 00700 00701 00702 1>4 00710 00711 00712 00713 2>5 00720 00721 00722 00723 00724",
		"line 30  3 0>4 00800 00801 00802 00803 1>5 00810 00811 00812 00813 00814 2>1 00820",
		"line 31  3 0>5 00900 00901 00902 00903 00904 1>1 00910 2>2 00920 00921",
		"line 32  4 0>1 00000 1>2 00010 00011 2>3 00020 00021 00022 3>4 00030 00031 00032 00033",
		"line 33  4 0>2 00100 00101 1>3 00110 00111 00112 2>4 00120 00121 00122 00123 3>5 00130 00131 00132 00133 00134",
		"line 34  4 0>3 00200 00201 00202 1>4 00210 00211 00212 00213 2>5 00220 00221 00222 00223 00224 3>1 00230",
		"line 35  4 0>4 00300 00301 00302 00303 1>5 00310 00311 00312 00313 00314 2>1 00320 3>2 00330 00331",
		"line 36  4 0>5 00400 00401 00402 00403 00404 1>1 00410 2>2 00420 00421 3>3 00430 00431 00432",
		"line 37  4 0>1 00500 1>2 00510 00511 2>3 00520 00521 00522 3>4 00530 00531 00532 00533",
		"line 38  4 0>2 00600 00601 1>3 00610 00611 00612 2>4 00620 00621 00622 00623 3>5 00630 00631 00632 00633 00634",
		"line 39  4 0>3 00700 00701 00702 1>4 00710 00711 00712 00713 2>5 00720 00721 00722 00723 00724 3>1 00730",
		"line 40  4 0>4 00800 00801 00802 00803 1>5 00810 00811 00812 00813 00814 2>1 00820 3>2 00830 00831",
		"line 41  4 0>5 00900 00901 00902 00903 00904 1>1 00910 2>2 00920 00921 3>3 00930 00931 00932",
	};
	private static String[] EXPECTED_XML2 = {
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 1</Test-id><Month-Count>0</Month-Count></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 2</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>0</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 3</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>100</daily-sales></week><week><Sep></Sep><daily-sales>101</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 4</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>200</daily-sales></week><week><Sep></Sep><daily-sales>201</daily-sales></week><week><Sep></Sep><daily-sales>202</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 5</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>300</daily-sales></week><week><Sep></Sep><daily-sales>301</daily-sales></week><week><Sep></Sep><daily-sales>302</daily-sales></week><week><Sep></Sep><daily-sales>303</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 6</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>400</daily-sales></week><week><Sep></Sep><daily-sales>401</daily-sales></week><week><Sep></Sep><daily-sales>402</daily-sales></week><week><Sep></Sep><daily-sales>403</daily-sales></week><week><Sep></Sep><daily-sales>404</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 7</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>500</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 8</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>600</daily-sales></week><week><Sep></Sep><daily-sales>601</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 9</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>700</daily-sales></week><week><Sep></Sep><daily-sales>701</daily-sales></week><week><Sep></Sep><daily-sales>702</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 10</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>800</daily-sales></week><week><Sep></Sep><daily-sales>801</daily-sales></week><week><Sep></Sep><daily-sales>802</daily-sales></week><week><Sep></Sep><daily-sales>803</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 11</Test-id><Month-Count>1</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>900</daily-sales></week><week><Sep></Sep><daily-sales>901</daily-sales></week><week><Sep></Sep><daily-sales>902</daily-sales></week><week><Sep></Sep><daily-sales>903</daily-sales></week><week><Sep></Sep><daily-sales>904</daily-sales></week></a4></Location-Details></CobolData>",
		
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 12</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>0</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>10</daily-sales></week><week><Sep></Sep><daily-sales>11</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 13</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>100</daily-sales></week><week><Sep></Sep><daily-sales>101</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>110</daily-sales></week><week><Sep></Sep><daily-sales>111</daily-sales></week><week><Sep></Sep><daily-sales>112</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 14</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>200</daily-sales></week><week><Sep></Sep><daily-sales>201</daily-sales></week><week><Sep></Sep><daily-sales>202</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>210</daily-sales></week><week><Sep></Sep><daily-sales>211</daily-sales></week><week><Sep></Sep><daily-sales>212</daily-sales></week><week><Sep></Sep><daily-sales>213</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 15</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>300</daily-sales></week><week><Sep></Sep><daily-sales>301</daily-sales></week><week><Sep></Sep><daily-sales>302</daily-sales></week><week><Sep></Sep><daily-sales>303</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>310</daily-sales></week><week><Sep></Sep><daily-sales>311</daily-sales></week><week><Sep></Sep><daily-sales>312</daily-sales></week><week><Sep></Sep><daily-sales>313</daily-sales></week><week><Sep></Sep><daily-sales>314</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 16</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>400</daily-sales></week><week><Sep></Sep><daily-sales>401</daily-sales></week><week><Sep></Sep><daily-sales>402</daily-sales></week><week><Sep></Sep><daily-sales>403</daily-sales></week><week><Sep></Sep><daily-sales>404</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>410</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 17</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>500</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>510</daily-sales></week><week><Sep></Sep><daily-sales>511</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 18</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>600</daily-sales></week><week><Sep></Sep><daily-sales>601</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>610</daily-sales></week><week><Sep></Sep><daily-sales>611</daily-sales></week><week><Sep></Sep><daily-sales>612</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 19</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>700</daily-sales></week><week><Sep></Sep><daily-sales>701</daily-sales></week><week><Sep></Sep><daily-sales>702</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>710</daily-sales></week><week><Sep></Sep><daily-sales>711</daily-sales></week><week><Sep></Sep><daily-sales>712</daily-sales></week><week><Sep></Sep><daily-sales>713</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 20</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>800</daily-sales></week><week><Sep></Sep><daily-sales>801</daily-sales></week><week><Sep></Sep><daily-sales>802</daily-sales></week><week><Sep></Sep><daily-sales>803</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>810</daily-sales></week><week><Sep></Sep><daily-sales>811</daily-sales></week><week><Sep></Sep><daily-sales>812</daily-sales></week><week><Sep></Sep><daily-sales>813</daily-sales></week><week><Sep></Sep><daily-sales>814</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 21</Test-id><Month-Count>2</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>900</daily-sales></week><week><Sep></Sep><daily-sales>901</daily-sales></week><week><Sep></Sep><daily-sales>902</daily-sales></week><week><Sep></Sep><daily-sales>903</daily-sales></week><week><Sep></Sep><daily-sales>904</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>910</daily-sales></week></a4></Location-Details></CobolData>",
		
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 22</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>0</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>10</daily-sales></week><week><Sep></Sep><daily-sales>11</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>20</daily-sales></week><week><Sep></Sep><daily-sales>21</daily-sales></week><week><Sep></Sep><daily-sales>22</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 23</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>100</daily-sales></week><week><Sep></Sep><daily-sales>101</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>110</daily-sales></week><week><Sep></Sep><daily-sales>111</daily-sales></week><week><Sep></Sep><daily-sales>112</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>120</daily-sales></week><week><Sep></Sep><daily-sales>121</daily-sales></week><week><Sep></Sep><daily-sales>122</daily-sales></week><week><Sep></Sep><daily-sales>123</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 24</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>200</daily-sales></week><week><Sep></Sep><daily-sales>201</daily-sales></week><week><Sep></Sep><daily-sales>202</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>210</daily-sales></week><week><Sep></Sep><daily-sales>211</daily-sales></week><week><Sep></Sep><daily-sales>212</daily-sales></week><week><Sep></Sep><daily-sales>213</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>220</daily-sales></week><week><Sep></Sep><daily-sales>221</daily-sales></week><week><Sep></Sep><daily-sales>222</daily-sales></week><week><Sep></Sep><daily-sales>223</daily-sales></week><week><Sep></Sep><daily-sales>224</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 25</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>300</daily-sales></week><week><Sep></Sep><daily-sales>301</daily-sales></week><week><Sep></Sep><daily-sales>302</daily-sales></week><week><Sep></Sep><daily-sales>303</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>310</daily-sales></week><week><Sep></Sep><daily-sales>311</daily-sales></week><week><Sep></Sep><daily-sales>312</daily-sales></week><week><Sep></Sep><daily-sales>313</daily-sales></week><week><Sep></Sep><daily-sales>314</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>320</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 26</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>400</daily-sales></week><week><Sep></Sep><daily-sales>401</daily-sales></week><week><Sep></Sep><daily-sales>402</daily-sales></week><week><Sep></Sep><daily-sales>403</daily-sales></week><week><Sep></Sep><daily-sales>404</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>410</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>420</daily-sales></week><week><Sep></Sep><daily-sales>421</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 27</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>500</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>510</daily-sales></week><week><Sep></Sep><daily-sales>511</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>520</daily-sales></week><week><Sep></Sep><daily-sales>521</daily-sales></week><week><Sep></Sep><daily-sales>522</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 28</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>600</daily-sales></week><week><Sep></Sep><daily-sales>601</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>610</daily-sales></week><week><Sep></Sep><daily-sales>611</daily-sales></week><week><Sep></Sep><daily-sales>612</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>620</daily-sales></week><week><Sep></Sep><daily-sales>621</daily-sales></week><week><Sep></Sep><daily-sales>622</daily-sales></week><week><Sep></Sep><daily-sales>623</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 29</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>700</daily-sales></week><week><Sep></Sep><daily-sales>701</daily-sales></week><week><Sep></Sep><daily-sales>702</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>710</daily-sales></week><week><Sep></Sep><daily-sales>711</daily-sales></week><week><Sep></Sep><daily-sales>712</daily-sales></week><week><Sep></Sep><daily-sales>713</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>720</daily-sales></week><week><Sep></Sep><daily-sales>721</daily-sales></week><week><Sep></Sep><daily-sales>722</daily-sales></week><week><Sep></Sep><daily-sales>723</daily-sales></week><week><Sep></Sep><daily-sales>724</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 30</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>800</daily-sales></week><week><Sep></Sep><daily-sales>801</daily-sales></week><week><Sep></Sep><daily-sales>802</daily-sales></week><week><Sep></Sep><daily-sales>803</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>810</daily-sales></week><week><Sep></Sep><daily-sales>811</daily-sales></week><week><Sep></Sep><daily-sales>812</daily-sales></week><week><Sep></Sep><daily-sales>813</daily-sales></week><week><Sep></Sep><daily-sales>814</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>820</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 31</Test-id><Month-Count>3</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>900</daily-sales></week><week><Sep></Sep><daily-sales>901</daily-sales></week><week><Sep></Sep><daily-sales>902</daily-sales></week><week><Sep></Sep><daily-sales>903</daily-sales></week><week><Sep></Sep><daily-sales>904</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>910</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>920</daily-sales></week><week><Sep></Sep><daily-sales>921</daily-sales></week></a4></Location-Details></CobolData>",
		
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 32</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>0</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>10</daily-sales></week><week><Sep></Sep><daily-sales>11</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>20</daily-sales></week><week><Sep></Sep><daily-sales>21</daily-sales></week><week><Sep></Sep><daily-sales>22</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>30</daily-sales></week><week><Sep></Sep><daily-sales>31</daily-sales></week><week><Sep></Sep><daily-sales>32</daily-sales></week><week><Sep></Sep><daily-sales>33</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 33</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>100</daily-sales></week><week><Sep></Sep><daily-sales>101</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>110</daily-sales></week><week><Sep></Sep><daily-sales>111</daily-sales></week><week><Sep></Sep><daily-sales>112</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>120</daily-sales></week><week><Sep></Sep><daily-sales>121</daily-sales></week><week><Sep></Sep><daily-sales>122</daily-sales></week><week><Sep></Sep><daily-sales>123</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>130</daily-sales></week><week><Sep></Sep><daily-sales>131</daily-sales></week><week><Sep></Sep><daily-sales>132</daily-sales></week><week><Sep></Sep><daily-sales>133</daily-sales></week><week><Sep></Sep><daily-sales>134</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 34</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>200</daily-sales></week><week><Sep></Sep><daily-sales>201</daily-sales></week><week><Sep></Sep><daily-sales>202</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>210</daily-sales></week><week><Sep></Sep><daily-sales>211</daily-sales></week><week><Sep></Sep><daily-sales>212</daily-sales></week><week><Sep></Sep><daily-sales>213</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>220</daily-sales></week><week><Sep></Sep><daily-sales>221</daily-sales></week><week><Sep></Sep><daily-sales>222</daily-sales></week><week><Sep></Sep><daily-sales>223</daily-sales></week><week><Sep></Sep><daily-sales>224</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>230</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 35</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>300</daily-sales></week><week><Sep></Sep><daily-sales>301</daily-sales></week><week><Sep></Sep><daily-sales>302</daily-sales></week><week><Sep></Sep><daily-sales>303</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>310</daily-sales></week><week><Sep></Sep><daily-sales>311</daily-sales></week><week><Sep></Sep><daily-sales>312</daily-sales></week><week><Sep></Sep><daily-sales>313</daily-sales></week><week><Sep></Sep><daily-sales>314</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>320</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>330</daily-sales></week><week><Sep></Sep><daily-sales>331</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 36</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>400</daily-sales></week><week><Sep></Sep><daily-sales>401</daily-sales></week><week><Sep></Sep><daily-sales>402</daily-sales></week><week><Sep></Sep><daily-sales>403</daily-sales></week><week><Sep></Sep><daily-sales>404</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>410</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>420</daily-sales></week><week><Sep></Sep><daily-sales>421</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>430</daily-sales></week><week><Sep></Sep><daily-sales>431</daily-sales></week><week><Sep></Sep><daily-sales>432</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 37</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>500</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>510</daily-sales></week><week><Sep></Sep><daily-sales>511</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>520</daily-sales></week><week><Sep></Sep><daily-sales>521</daily-sales></week><week><Sep></Sep><daily-sales>522</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>530</daily-sales></week><week><Sep></Sep><daily-sales>531</daily-sales></week><week><Sep></Sep><daily-sales>532</daily-sales></week><week><Sep></Sep><daily-sales>533</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 38</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>600</daily-sales></week><week><Sep></Sep><daily-sales>601</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>610</daily-sales></week><week><Sep></Sep><daily-sales>611</daily-sales></week><week><Sep></Sep><daily-sales>612</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>620</daily-sales></week><week><Sep></Sep><daily-sales>621</daily-sales></week><week><Sep></Sep><daily-sales>622</daily-sales></week><week><Sep></Sep><daily-sales>623</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>630</daily-sales></week><week><Sep></Sep><daily-sales>631</daily-sales></week><week><Sep></Sep><daily-sales>632</daily-sales></week><week><Sep></Sep><daily-sales>633</daily-sales></week><week><Sep></Sep><daily-sales>634</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 39</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>700</daily-sales></week><week><Sep></Sep><daily-sales>701</daily-sales></week><week><Sep></Sep><daily-sales>702</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>710</daily-sales></week><week><Sep></Sep><daily-sales>711</daily-sales></week><week><Sep></Sep><daily-sales>712</daily-sales></week><week><Sep></Sep><daily-sales>713</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>720</daily-sales></week><week><Sep></Sep><daily-sales>721</daily-sales></week><week><Sep></Sep><daily-sales>722</daily-sales></week><week><Sep></Sep><daily-sales>723</daily-sales></week><week><Sep></Sep><daily-sales>724</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>730</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 40</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>4</week-of-month><week><Sep></Sep><daily-sales>800</daily-sales></week><week><Sep></Sep><daily-sales>801</daily-sales></week><week><Sep></Sep><daily-sales>802</daily-sales></week><week><Sep></Sep><daily-sales>803</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>810</daily-sales></week><week><Sep></Sep><daily-sales>811</daily-sales></week><week><Sep></Sep><daily-sales>812</daily-sales></week><week><Sep></Sep><daily-sales>813</daily-sales></week><week><Sep></Sep><daily-sales>814</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>820</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>830</daily-sales></week><week><Sep></Sep><daily-sales>831</daily-sales></week></a4></Location-Details></CobolData>",
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?><CobolData><Location-Details><Test-id>line 41</Test-id><Month-Count>4</Month-Count><a4><text> 0&gt;</text><week-of-month>5</week-of-month><week><Sep></Sep><daily-sales>900</daily-sales></week><week><Sep></Sep><daily-sales>901</daily-sales></week><week><Sep></Sep><daily-sales>902</daily-sales></week><week><Sep></Sep><daily-sales>903</daily-sales></week><week><Sep></Sep><daily-sales>904</daily-sales></week></a4><a4><text> 1&gt;</text><week-of-month>1</week-of-month><week><Sep></Sep><daily-sales>910</daily-sales></week></a4><a4><text> 2&gt;</text><week-of-month>2</week-of-month><week><Sep></Sep><daily-sales>920</daily-sales></week><week><Sep></Sep><daily-sales>921</daily-sales></week></a4><a4><text> 3&gt;</text><week-of-month>3</week-of-month><week><Sep></Sep><daily-sales>930</daily-sales></week><week><Sep></Sep><daily-sales>931</daily-sales></week><week><Sep></Sep><daily-sales>932</daily-sales></week></a4></Location-Details></CobolData>",
	};

	

//	public void testConversionToXml1() throws IOException, XMLStreamException {
//		String copybookFileName = TstXmlConstants.COBOL_DIRECTORY + "OccursDependingOn61b.cbl";
//
//		
//		ICobol2Xml cbl2Xml = Cobol2Xml.newCobol2Xml(copybookFileName)
//				.setDialect(ICopybookDialects.FMT_GNU_COBOL);
//		ISchemaIOBuilder iob = cbl2Xml.asIOBuilder();
//		int k = 1;
//		
//		AbstractLine line = iob.newLine();
//		line.setData(LINES[0]);
//		line.getFieldValue("Test-id").set("line " + k++);
//		line.getFieldValue("Month-Count").set(0);
//		ByteArrayOutputStream os = new ByteArrayOutputStream();
//		cbl2Xml.cobol2xml(new ByteArrayInputStream(line.getData()), os);
//
//		System.out.println(new String(os.toByteArray()));
//		for (int j = 1; j < 5; j++) {
//			for (int i = 0; i < 10; i++) {
//				line = iob.newLine();
//				line.setData(LINES[i]);
//				line.getFieldValue("Test-id").set("line " + k++);
//				line.getFieldValue("Month-Count").set(j);
//				os = new ByteArrayOutputStream();
//				cbl2Xml.cobol2xml(new ByteArrayInputStream(line.getData()), os);
//
//				System.out.println(new String(os.toByteArray()));
//			}
//		}
//		
//	}
//	

	public void testConversionToXml2() throws IOException, XMLStreamException {
		String copybookFileName = TstXmlConstants.COBOL_DIRECTORY + "OccursDependingOn61b.cbl";

		
		ICobol2Xml cbl2Xml = Cobol2Xml.newCobol2Xml(copybookFileName)
				.setDialect(ICopybookDialects.FMT_GNU_COBOL);
		
		for (int i = 0; i < LINES2.length; i++) {
			ByteArrayOutputStream os = new ByteArrayOutputStream();
			cbl2Xml.cobol2xml(new ByteArrayInputStream(LINES2[i].getBytes()), os);

			assertEquals(EXPECTED_XML2[i], new String(os.toByteArray()));

//			assertEquals(EXPECTED_XML[i], new String(os.toByteArray()));
//			System.out.println();
//			System.out.println(LINES[i]);
//			System.out.println(new String(os.toByteArray()));
		}
	}


	public void testXmlToCobol2() throws IOException, XMLStreamException {
		String copybookFileName = TstXmlConstants.COBOL_DIRECTORY + "OccursDependingOn61b.cbl";
		
		ICobol2Xml cbl2Xml = Cobol2Xml.newCobol2Xml(copybookFileName)
				.setDialect(ICopybookDialects.FMT_GNU_COBOL);
		
		for (int i = 0; i < EXPECTED_XML2.length; i++) {
			ByteArrayOutputStream os = new ByteArrayOutputStream();
			cbl2Xml.xml2Cobol(new ByteArrayInputStream(EXPECTED_XML2[i].getBytes()), os);
			
			String actual = new String(os.toByteArray());
			while (actual.endsWith("\n") || actual.endsWith("\r")) {
				actual = actual.substring(0, actual.length() - 1);
			}
			assertEquals("> " + i, LINES2[i], actual);
			//System.out.println(actual);
		}


	}

}
