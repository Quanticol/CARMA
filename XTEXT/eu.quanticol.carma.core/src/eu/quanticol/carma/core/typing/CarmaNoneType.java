/**
 * 
 */
package eu.quanticol.carma.core.typing;

/**
 * @author loreti
 *
 */
public class CarmaNoneType extends CarmaType {

	protected CarmaNoneType() {
		super(CarmaType.TypeCode.NONE);
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		return obj instanceof CarmaNoneType;
	}

}
