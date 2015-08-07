/**
 * 
 */
package eu.quanticol.carma.core.typing;

import org.eclipse.emf.ecore.EObject;

import eu.quanticol.carma.core.carma.EnumDefinition;
import eu.quanticol.carma.core.carma.RecordDefinition;
import eu.quanticol.carma.core.utils.Util;

/**
 * @author loreti
 *
 */
public class CarmaType {
	
	public final static CarmaType INTEGER_TYPE = new CarmaType( TypeCode.INTEGER , null );
	public final static CarmaType REAL_TYPE = new CarmaType( TypeCode.REAL , null );
	public final static CarmaType BOOLEAN_TYPE = new CarmaType( TypeCode.BOOLEAN , null );
	public final static CarmaType ERROR_TYPE = new CarmaType( TypeCode.ERROR , null );
	public final static CarmaType PROCESS_TYPE = new CarmaType( TypeCode.PROCESS , null );
	
	public static CarmaType createRecordType( RecordDefinition reference ) {
		return new CarmaType( TypeCode.RECORD , reference );
	}

	public static CarmaType createEnumType( EnumDefinition reference ) {
		return new CarmaType( TypeCode.ENUM , reference );
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

	private final EObject reference;
	
	public CarmaType( TypeCode code , EObject reference ) {
		this.code = code;
		this.reference = reference;
	}
	
	public TypeCode getCode() {
		return code;
	}
	
	public EObject getReference() {
		return reference;
	}
	
	public boolean isError() {
		return code == TypeCode.ERROR;
	}

	public boolean isBoolean() {
		return code == TypeCode.BOOLEAN;
	}

	public boolean isNumber() {
		return (code == TypeCode.INTEGER)||(code == TypeCode.REAL);
	}

	public boolean isInteger() {
		return (code == TypeCode.INTEGER);
	}

	public boolean isReal() {
		return (code == TypeCode.REAL);
	}

	public boolean isRecord() {
		return code == TypeCode.RECORD;
	}
	
	public boolean isProcess() {
		return code == TypeCode.PROCESS;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof CarmaType) {
			CarmaType cType = (CarmaType) obj;
			return (this.code == cType.code)&&
					(((this.reference == null)&&(cType.reference==null))||
					((this.reference != null)&&(this.reference.equals(cType.reference))));
		}
		return false;
	}

	@Override
	public int hashCode() {
		return code.hashCode();
	}

	@Override
	public String toString() {
		return code.toString();
	}
	
	
	public CarmaType mostGeneral( CarmaType t ) {
		if ( t==null ) {
			return null;
		}
		if (this.equals(t)) {
			return this;
		}
		if (((this.code == TypeCode.INTEGER)&&(t.code==TypeCode.REAL))
			||((this.code == TypeCode.REAL)&&(t.code == TypeCode.INTEGER))) {
			return REAL_TYPE;
		}
		return ERROR_TYPE;
	}
	
	public boolean isCompatibleWith( CarmaType t ) {
		if (t == null) {
			return false;
		}
		if (this.equals(t)) {
			return true;
		}
		return (((this.code == TypeCode.INTEGER)&&(t.code==TypeCode.REAL))
				||((this.code == TypeCode.REAL)&&(t.code == TypeCode.INTEGER)));
	}

	
}
