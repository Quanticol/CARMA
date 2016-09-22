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
public class CarmaBooleanType extends CarmaType {
	
	public CarmaBooleanType() {
		super( CarmaType.TypeCode.BOOLEAN );
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		return obj instanceof CarmaBooleanType;
	}



	
}
