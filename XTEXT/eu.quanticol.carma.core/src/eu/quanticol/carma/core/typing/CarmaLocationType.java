/**
 * 
 */
package eu.quanticol.carma.core.typing;

/**
 * @author loreti
 *
 */
public class CarmaLocationType extends CarmaType {
	
	protected CarmaLocationType( ) {
		super(CarmaType.TypeCode.LOCATION);
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		return obj instanceof CarmaLocationType;
	}

}
