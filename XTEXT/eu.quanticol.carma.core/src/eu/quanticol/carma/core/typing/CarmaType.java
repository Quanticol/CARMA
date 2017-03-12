/**
 * 
 */
package eu.quanticol.carma.core.typing;

import java.util.LinkedList;

import org.eclipse.emf.ecore.EObject;

import eu.quanticol.carma.core.carma.EnumDefinition;
import eu.quanticol.carma.core.carma.RecordDefinition;
import eu.quanticol.carma.core.utils.Util;

/**
 * @author loreti
 *
 */
public abstract class CarmaType {
	
	public final static CarmaIntegerType INTEGER_TYPE = new CarmaIntegerType( );
	public final static CarmaRealType REAL_TYPE = new CarmaRealType( );
	public final static CarmaBooleanType BOOLEAN_TYPE = new CarmaBooleanType( );
	public final static CarmaType ERROR_TYPE = new CarmaErrorType( );
	public final static CarmaProcessType PROCESS_TYPE = new CarmaProcessType( );
	public final static CarmaLocationType LOCATION_TYPE = new CarmaLocationType();	
	public final static CarmaNoneType NONE_TYPE = new CarmaNoneType();
	public final static CarmaEdgeType EDGE_TYPE = new CarmaEdgeType();
	
	public static CarmaType createRecordType( RecordDefinition reference ) {
		return new CarmaRecordType( reference );
	}
	

	public static CarmaEnumType createEnumType( EnumDefinition reference ) {
		return new CarmaEnumType( reference );
	}

	public static CarmaSetType createSetType( CarmaType t ) {
		return new CarmaSetType( t );
	}
	
	public static CarmaListType createListType( CarmaType t ) {
		return new CarmaListType(t);
	}
	
	public static CarmaTupleType createTupleType( CarmaType ... types ) {
		return new CarmaTupleType( types );
	}
	
	public static CarmaFunctionType createFunctionType( CarmaType argument , CarmaType result ) {
		return new CarmaFunctionType( argument, result );
	}
	
	public static enum TypeCode {
		SET ,
		LIST ,
		LOCATION ,
		BOOLEAN , 
		INTEGER ,
		REAL ,
		ENUM ,
		RECORD ,
		PROCESS ,
		FUNCTION ,
		TUPLE ,
		NONE ,
		ERROR, EDGE
	}
	
	private final TypeCode code;
	
	protected CarmaType( TypeCode code ) {
		this.code = code;
	}
	
	public TypeCode getCode() {
		return code;
	}
	
	public boolean isError() {
		return code == TypeCode.ERROR;
	}

	public boolean isNone() {
		return code == TypeCode.NONE;
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
	
	public boolean isSet() {
		return code == TypeCode.SET;
	}

	public boolean isList() {
		return code == TypeCode.LIST;
	}
	
	public boolean isFunction() {
		return code == TypeCode.FUNCTION;
	}
	
	public boolean isLocation() {
		return code == TypeCode.LOCATION;
	}
	
	
	public boolean isEdge() {
		return code == TypeCode.EDGE;
	}
	
	public CarmaErrorType asError() {
		return (CarmaErrorType) this;
	}
	
	public CarmaFunctionType asFunction() {
		return (CarmaFunctionType) this;
	}
	
	public CarmaRecordType asRecord() {
		return (CarmaRecordType) this;
	}

	public CarmaEnumType asEnum() {
		return (CarmaEnumType) this;
	}

	public CarmaListType asList() {
		return (CarmaListType) this;
	}

	public CarmaSetType asSet() {
		return (CarmaSetType) this;
	}

	public CarmaTupleType asTuple() {
		return (CarmaTupleType) this;
	}



	@Override
	public boolean equals(Object obj) {
		if (obj instanceof CarmaType) {
			return doEquals( (CarmaType) obj );
		}
		return false;
	}

	protected abstract boolean doEquals(CarmaType obj);

	@Override
	public int hashCode() {
		return code.hashCode();
	}

	@Override
	public String toString() {
		return doToString();
	}
	
	
	protected String doToString() {
		return code.toString();
	}

	public CarmaType mostGeneral( CarmaType t ) {
		if ( t==null ) {
			return null;
		}
		if (this.equals(t)) {
			return this;
		}
//		if (((this.code == TypeCode.INTEGER)&&(t.code==TypeCode.REAL))
//			||((this.code == TypeCode.REAL)&&(t.code == TypeCode.INTEGER))) {
//			return REAL_TYPE;
//		}
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
