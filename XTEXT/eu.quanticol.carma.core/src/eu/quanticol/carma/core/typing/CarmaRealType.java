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
public class CarmaRealType extends CarmaType {
	

	
	public CarmaRealType( ) {
		super( CarmaType.TypeCode.REAL );
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		return obj instanceof CarmaRealType;
	}
	


	
}
