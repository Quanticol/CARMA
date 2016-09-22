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
public class CarmaIntegerType extends CarmaType {
	
	public CarmaIntegerType( ) {
		super( CarmaType.TypeCode.INTEGER );
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		return (obj instanceof CarmaIntegerType);
	}

	

	
}
