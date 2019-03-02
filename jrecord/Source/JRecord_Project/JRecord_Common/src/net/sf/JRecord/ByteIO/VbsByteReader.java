/*
1 * Purpose: Record oriented reading of Variable Record Length Binary files
 *
 * @Author Bruce Martin
 * Created on 27/08/2005
 *
 */
/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord Common
 *    
 *    Sub-Project purpose: Common Low-Level Code shared between 
 *                        the JRecord and Record Projects
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
      
package net.sf.JRecord.ByteIO;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;




/**
 * This class performs record oriented reading of:
 * </ul compact>
 *   <li>Mainframe Variable Record Length Binary files
 *   <li>Mainframe Variable Record Length Binary files with imbedded block lengths
 * </ul>
 *
 * into Array of Bytes
 *
 * @author Bruce Martin
 * @version 0.55
 *
 */
public class VbsByteReader extends AbstractByteReader {

//	private static final int LAST_7_BITS_SET = 127;

    private InputStream inStream;
	private BufferedInputStream stream = null;

	private int lineNumber = 0;
	private int rdwAdjust = 0;


	/**
	 * record descriptor word, it consists of
	 * 2 bytes length
	 * 2 bytes (hex zero)
	 */
	private byte[] rdw = new byte[4];
//	private byte[] rdwLength = new byte[2];


//	private boolean containsBlockLength = false;
//	private byte[] bdw = new byte[4];
//	private byte[] bdwLength = new byte[2];


//	private int blockLength = -1;
//	private int bytesReadFromBlock = 0;
	
	private byte[] nextRecord = null;
	
	private ArrayList<byte[]> subLines = new ArrayList<byte[]>(5);



	public VbsByteReader(final boolean includesBlockLength, boolean lengthIncludesRDW) {
	    super();

//	    containsBlockLength = includesBlockLength;
	    if (lengthIncludesRDW) {
	    	rdwAdjust = 4;
	    }
	}

	/**
     * @see AbstractByteReader#open(java.io.InputStream)
     */
    public void open(InputStream inputStream) {

        inStream = inputStream;

        if (inputStream instanceof BufferedInputStream) {
        	stream = (BufferedInputStream) inputStream;
        } else {
        	stream = new BufferedInputStream(inputStream, BUFFER_SIZE);
        }

 //       checkForBlockLength();
    }


    /**
     * @see AbstractByteReader#read()
     */
    public byte[] read()  throws IOException {
        byte[] ret = nextRecord;
        
        if (ret != null) {
        	nextRecord = null;
        	return ret;
        }

        if (stream == null) {
            throw new IOException(AbstractByteReader.NOT_OPEN_MESSAGE);
        }

//        checkForBlockLength();

        lineNumber += 1;
        ret = readLinePart();
        if (rdw[2] != 0 && ret != null) {
        	byte[] t = ret;
        	subLines.clear();
        	do {
        		subLines.add(t);
        		t = readLinePart();
        		if (rdw[2] == 0) {
        			nextRecord = t;
        			return join(subLines);
        		}
        		
        	} while (rdw[2] != 2);
        	subLines.add(t);
        	ret = join(subLines);
        }

        return ret;
    }

    /**
     * join a list of line parts to form a single line.
     * it is protected for testing
     * 
     * @param lineParts list of byte arrays to be joined
     * 
     * @return Single line made from combining the parts.
     */
    protected final byte[] join(ArrayList<byte[]> lineParts) {
		int len = 0;
		
		for (byte[] b : lineParts) {
			len += b.length;
		}
		
		byte[] ret = new byte[len];
		int pos = 0;
		
		for (byte[] b : lineParts) {
			System.arraycopy(b, 0, ret, pos, b.length);
			pos += b.length;
		}
		
		return ret;
   }
    
	/**
	 * @param ret
	 * @return
	 * @throws IOException
	 */
	public byte[] readLinePart() throws IOException {
		byte[] ret = null;
		if (readBuffer(stream, rdw) > 0) {
        	int lineLength = ((rdw[0] & 0xFF) << 8) + (rdw[1] & 0xFF) - rdwAdjust;
            if (rdw[2] < 0 || rdw[2] > 3  || rdw[3] != 0) {
//              if ((rdw[2] != 0 &&  rdw[2] != 1 && rdw[2] != 2) || rdw[3] != 0) {
                throw new IOException(
                          "Invalid Record Descriptor word at line "
                        + lineNumber + " " + lineLength + "\tRDW=" + rdw[2] + ", " + rdw[3]
                      );
            }

        	if (lineLength < 0) {
        		throw new IOException("Invalid Line Length: " + lineLength + " For line " + lineNumber); 
        	}
            byte[] inBytes = new byte[lineLength];

            if (readBuffer(stream, inBytes) >= 0) {
                ret = inBytes;

//                if (containsBlockLength) {
//                	bytesReadFromBlock += lineLength + 4;
//                }
			}
        }
		return ret;
	}


//    /**
//     * reads the blocklength if necessary
//     */
//    private void checkForBlockLength() {
//
//        if (containsBlockLength && bytesReadFromBlock >= blockLength) {
//        	try {
//    			bytesReadFromBlock = 4;
//        		if ((readBuffer(stream, bdw) > 0)) {
//        			if (bdw[0] >= 0) {
////        	            bdwLength[0] = bdw[0];
////        	            bdwLength[1] = bdw[1];
////
////        	        	blockLength = (new BigInteger(bdwLength)).intValue();
//        	        	blockLength = ((bdw[0] & 0xFF) << 8) + (bdw[1] & 0xFF);
//        			} else {
//        				bdw[0] &= LAST_7_BITS_SET;
//        				blockLength = ((bdw[0] & 0xFF) << 24) + ((bdw[1] & 0xFF) << 16) 
//								+ ((bdw[2] & 0xFF) << 8) + ((bdw[3] & 0xFF));
// //       	        	blockLength = (new BigInteger(bdw)).intValue();
//        			}
//        		}
//        	} catch (Exception e) {
//				e.printStackTrace();
//			}
//        }
//    }

    /**
     * @see AbstractByteReader#close()
     */
    public void close() throws IOException {

        inStream.close();
        stream = null;
    }

}
