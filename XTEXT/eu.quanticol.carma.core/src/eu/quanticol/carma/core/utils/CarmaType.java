/**
 * 
 */
package eu.quanticol.carma.core.utils;

/**
 * @author loreti
 *
 */
public class CarmaType {
	
	public final static CarmaType INTEGER_TYPE = new CarmaType( TypeCode.INTEGER , "int" );
	public final static CarmaType REAL_TYPE = new CarmaType( TypeCode.REAL , "real" );
	public final static CarmaType BOOLEAN_TYPE = new CarmaType( TypeCode.BOOLEAN , "bool" );
	public final static CarmaType ERROR_TYPE = new CarmaType( TypeCode.ERROR , "error" );
	public final static CarmaType PROCESS_TYPE = new CarmaType( TypeCode.PROCESS , "process" );

	public static CarmaType createRecordType( String name ) {
		return new CarmaType( TypeCode.RECORD , name );
	}

	public static CarmaType createEnumType( String name ) {
		return new CarmaType( TypeCode.ENUM , name );
	}

	public static enum TypeCode {
		BOOLEAN , 
		INTEGER ,
		REAL ,
		ENUM ,
		RECORD ,
		PROCESS ,
		ERROR
	}
	
	private final TypeCode code;

	private final String id;
	
	public CarmaType( TypeCode code , String id ) {
		this.code = code;
		this.id = id;
	}
	
	public TypeCode getCode() {
		return code;
	}
	
	public String getId() {
		return id;
	}
	
	public boolean isError() {
		return code == TypeCode.ERROR;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof CarmaType) {
			CarmaType cType = (CarmaType) obj;
			return (this.code == cType.code)&&(this.id.equals(cType.id));
		}
		return false;
	}

	@Override
	public int hashCode() {
		return id.hashCode();
	}

	@Override
	public String toString() {
		return id;
	}
	
	
	public CarmaType mostGeneral( CarmaType t ) {
		if (this.equals(t)) {
			return this;
		}
		if (((this.code == TypeCode.INTEGER)&&(t.code==TypeCode.REAL))
			||((this.code == TypeCode.REAL)&&(t.code == TypeCode.INTEGER))) {
			return REAL_TYPE;
		}
		return ERROR_TYPE;
	}
	
}
