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
public class CarmaEnumType extends CarmaType {
	
	private EnumDefinition reference;

	public CarmaEnumType(EnumDefinition reference) {
		super( CarmaType.TypeCode.ENUM);
		this.reference = reference;
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		if (obj instanceof CarmaEnumType) {
			return this.reference.equals( ((CarmaEnumType) obj).reference );
		}
		return false;
	}

	@Override
	protected String doToString() {
		return reference.toString();
	}
	
	public EnumDefinition getReference() {
		return reference;
	}
	

	
}
