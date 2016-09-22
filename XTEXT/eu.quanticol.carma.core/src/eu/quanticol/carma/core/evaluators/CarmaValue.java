/**
 * 
 */
package eu.quanticol.carma.core.evaluators;

/**
 * @author loreti
 *
 */
public interface CarmaValue {
	
	//Checking value types.
	boolean isBoolean();
	boolean isInteger();
	boolean isReal();
	boolean isRecord();
	boolean isEnum();
	boolean isNone();
	
	//Collecting values:
	boolean getBooleanValue();
	int getIntegerValue();
	double getRealValue();
	CarmaValue getFieldValue( String name );
	

	//Boolean operations.
	boolean isTrue();	
	boolean isFalse();
	CarmaValue and( CarmaValue v );
	CarmaValue or( CarmaValue v );
	CarmaValue not( );
	
	
	//Arithmetic Operations
	CarmaValue plus( CarmaValue v );	
	CarmaValue minus( CarmaValue v );	
	CarmaValue mul( CarmaValue v );
	CarmaValue div( CarmaValue v );
	
	//Comparison Operations
	CarmaValue lessThan( CarmaValue v );	
	CarmaValue lessOrEqualThan( CarmaValue v );
	CarmaValue equalTo( CarmaValue v );
	CarmaValue notEqualTo( CarmaValue v );
	CarmaValue greaterThan( CarmaValue v );
	CarmaValue greaterOrEqualThan( CarmaValue v );
		
	//Math operations
	CarmaValue abs( );
	CarmaValue cos( );
	CarmaValue acos( );
	CarmaValue asin( );
	CarmaValue atan( );
	CarmaValue atan2(CarmaValue v );
	CarmaValue cbrt( );
	CarmaValue ceil( );
	CarmaValue exp( );
	CarmaValue floor( );
	CarmaValue log( );
	CarmaValue log10( );
	CarmaValue max( CarmaValue v );
	CarmaValue min( CarmaValue v );
	CarmaValue pow( CarmaValue v );
	CarmaValue sin( );
	CarmaValue sqrt( );
	CarmaValue tan( );
	
}
