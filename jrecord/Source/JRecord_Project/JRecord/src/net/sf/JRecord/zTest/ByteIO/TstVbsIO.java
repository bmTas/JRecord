/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.zTest.ByteIO;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import net.sf.JRecord.ByteIO.VbsByteReader;
import junit.framework.TestCase;

/**
 * Testing the Vbs (Variable blocked scanned) byte-reader class
 * 
 * @author Bruce Martin
 *
 */
public class TstVbsIO extends TestCase {

	final static byte[][] byteLines1 = {
			{0, 1},
			{2, 3, 4, 5},
			{6},
			{7, 8, 9},
			{10, 11, 12, 13, 14, 15},
			{16, 17, 18},
			{19},
			{20, 21, 22, 23, 24, 25, 26, 27, 28, 29},
			{30, 31, 32},
			{33, 34, 35, 36, 37, 38, 39, 40},
			{41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51},
			{52, 53, 54}
			
	};
	
	final static byte[][] byteLines2 = {
		{0},
		{1,2,3,4,5},
		{6},
		{7, 8, 9},
		{10},
		{11, 12, 13, 14}, 
		{15},
		{16, 17, 18},
		{19},
		{20, 21, 22, 23, 24, 25, 26, 27, 28, 29},
	};
	
	
	final static byte[][] byteLines3 = {
		{},
		{0, 1,2,3,4,5,6},
		{},
		{7, 8, 9},
		{},
		{10, 11, 12, 13, 14, 15}, 
		{},
		{16, 17, 18},
		{},
		{19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29},
	};
	
	public void testJoin1() {
		tstJoin(byteLines1);
	}
	
	public void testJoin2() {
		tstJoin(byteLines2);
	}
	
	public void testJoin3() {
		tstJoin(byteLines3);
	}

	/**
	 * 
	 */
	public void tstJoin(byte[][] byteLines) {
		ArrayList<byte[]> subLines = new ArrayList<byte[]>(5);
		Vbsr vbsr = new Vbsr();

		byte[] bj;
		for (int i = 0; i < byteLines.length; i++) {
			int len = 0;
			subLines.clear();
			for (int j = 0; j <= i; j++) {
				subLines.add(byteLines[j]);
				len += byteLines[j].length;
			}
			
			bj = vbsr.doJoin(subLines);
			
			assertEquals(len, bj.length);
			
			for (int j = 0; j < bj.length; j++) {
				assertEquals(i + ",  " + j, j, bj[j]);
			}
		}
	}
	
	public void testRead1a() throws IOException {
		tstRead1(byteLines1);
	}
	
	public void testRead1b() throws IOException {
		tstRead1(byteLines2);
	}
	
	public void testRead1c() throws IOException {
		tstRead1(byteLines3);
	}

	/**
	 * @throws IOException
	 */
	public void tstRead1(byte[][] byteLines) throws IOException {
		Data data = new Data();

		addStdLines1(data, false, byteLines);
		
		data.check();
		
		data = new Data();

		addStdLines1(data, true, byteLines);
		
		data.check();
	}
	
	
	public void testRead2() throws IOException {
		Data data = new Data();

		addStdLines1(data, false, byteLines1);
		addStdLines1(data, true, byteLines1);
		addStdLines2(data, 1, byteLines1);
		
		data.check();
	}

	
	public void testRead3() throws IOException {
		Data data = new Data();

		tstRead3(data, byteLines1);
		tstRead3(data, byteLines2);
		tstRead3(data, byteLines3);
	
		data.check();
	}


	
	public void testRead4() throws IOException {
		Data data = new Data();

		tstRead4(data, byteLines1);
		tstRead4(data, byteLines2);
		tstRead4(data, byteLines3);
	
		data.check();
	}

	/**
	 * @param data
	 * @throws IOException
	 */
	public void tstRead4(Data data, byte[][] byteLines) throws IOException {
		tstRead3(data, byteLines1);
		addStdLines2(data, 4, byteLines);
		addStdLines2(data, 5, byteLines);
	}

	/**
	 * @param data
	 * @throws IOException
	 */
	public void tstRead3(Data data, byte[][] byteLines) throws IOException {
		addStdLines1(data, false, byteLines);
		addStdLines1(data, true, byteLines);
		addStdLines2(data, 1, byteLines);
		addStdLines2(data, 2, byteLines);
		addStdLines2(data, 3, byteLines);
	}

	/**
	 * @param data
	 * @throws IOException
	 */
	public void addStdLines1(Data data, boolean addLine, byte[][] byteLines) throws IOException {
		ArrayList<byte[]> subLines = new ArrayList<byte[]>(5);
		for (int i = 0; i < byteLines.length; i++) {
			subLines.clear();
			for (int j = 0; j <= i; j++) {
				subLines.add(byteLines[j]);
			}
			
			data.writeLines(subLines);
			if (addLine) {
				data.writeAline(byteLines[i]);
			}
		}
	}
	

	/**
	 * @param data
	 * @throws IOException
	 */
	public void addStdLines2(Data data, int adj, byte[][] byteLines) throws IOException {
		ArrayList<byte[]> subLines = new ArrayList<byte[]>(5);
		for (int i = 0; i < byteLines.length; i++) {
			subLines.clear();
			for (int j = Math.max(0, i-adj); j <= i; j++) {
				subLines.add(byteLines[j]);
			}
			
			data.writeLines(subLines);
			if (i % 2 == 0) {
				data.writeAline(byteLines[i]);
			}

		}
	}


	private static class Data {
		Vbsr rr = new Vbsr();
		ArrayList<byte[]> linesWritten = new ArrayList<byte[]>();
		ArrayList<ArrayList<byte[]>> rawData = new ArrayList<ArrayList<byte[]>>(20);
		
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		
		void writeAline(byte[] line) throws IOException {
			os.write(new byte[] {0, (byte)(line.length+4), 0, 0});
			os.write(line);
			
			linesWritten.add(line);
			
			ArrayList<byte[]> lines = new ArrayList<byte[]>(1);
			lines.add(line);
			rawData.add(lines);
		}
		
		void writeLines(ArrayList<byte[]> lines) throws IOException {
			switch (lines.size()) {
			case 0: return;
			case 1: writeAline(lines.get(0));		return;
			}
			os.write(new byte[] {0, (byte)(lines.get(0).length+4), 1, 0});
			os.write(lines.get(0));
			int lastIdx = lines.size() - 1;
			for (int i = 1; i < lastIdx; i++) {
				os.write(new byte[] {0, (byte)(lines.get(i).length+4), 3, 0});
				os.write(lines.get(i));
			}
			os.write(new byte[] {0, (byte)(lines.get(lastIdx).length+4), 2, 0});
			os.write(lines.get(lastIdx));
			
			linesWritten.add(rr.doJoin(lines));
			rawData.add(new ArrayList<byte[]>(lines));
		}

		void check() throws IOException {
			byte[] line;
			int i = 0;
			os.close();
			rr.open(new ByteArrayInputStream(os.toByteArray()));
			
			while ((line = rr.read()) != null) {
				boolean ok = linesWritten.get(i).length == line.length;
				
				for (int j = 0; ok && j < line.length; j++) {
					ok = linesWritten.get(i)[j] == line[j];
				}
				if (!ok) {
					System.out.println();
					System.out.println();
					for (int j = 0; j < line.length; j++) {
						System.out.print("\t" + line[j]);
					}
					System.out.println();
					for (int j = 0; j < linesWritten.get(i).length; j++) {
						System.out.print("\t" + linesWritten.get(i)[j]);
					}
					System.out.println();
				}
				assertTrue(i + ", " + rawData.get(i).size(), ok);
				
				i += 1;
			}
			assertEquals(linesWritten.size(), i);
			rr.close();
		}
	}
	
	private static class Vbsr extends VbsByteReader {

		protected Vbsr() {
			super(false, true);
		}
	
		byte[] doJoin(ArrayList<byte[]> lineParts) {
			return super.join(lineParts);
		}
	}
}
