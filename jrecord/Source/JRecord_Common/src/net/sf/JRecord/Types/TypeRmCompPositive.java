package net.sf.JRecord.Types;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;

public class TypeRmCompPositive extends TypeNum {

	public TypeRmCompPositive() {
		super(false, true, true, true, true);
	}



	@Override
	public Object getField(byte[] record, int position, IFieldDetail currField) {
		Object ret;
		long retL = getRmComp(record, position, currField.getLen());
		
		ret = Long.valueOf(retL);
		
		if (currField.getDecimal() > 0) {
			ret = new BigDecimal(BigInteger.valueOf(retL), currField.getDecimal());
		}
		
		return ret;
	}
	
	@Override
	public byte[] setField(byte[] record, int position, IFieldDetail field,
			Object value) throws RecordException {

		
		String val = value.toString();
			
		formatValueForRecord(field, val);
		
		return setRmComp(record, position, field.getLen(), Math.abs(getBigDecimal(field, val).longValue()));
	}

}
