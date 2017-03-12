/**
 * 
 */
package eu.quanticol.carma.core.typing;

/**
 * @author loreti
 *
 */
public class CarmaEdgeType extends CarmaType {

	protected CarmaEdgeType( ) {
		super(CarmaType.TypeCode.EDGE);
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		return obj instanceof CarmaEdgeType;
	}

}
