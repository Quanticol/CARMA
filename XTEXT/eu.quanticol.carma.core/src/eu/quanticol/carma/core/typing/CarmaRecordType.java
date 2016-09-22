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
public class CarmaRecordType extends CarmaType {

	private final RecordDefinition reference;

	public CarmaRecordType(RecordDefinition reference) {
		super( CarmaType.TypeCode.RECORD );
		this.reference = reference;
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		if (obj instanceof CarmaRecordType) {
			CarmaRecordType other = (CarmaRecordType) obj;
			return this.reference.equals(other.reference);
		}
		return false;
	}

	@Override
	protected String doToString() {
		return reference.getName();
	}

	public RecordDefinition getReference() {
		return reference;
	}
	
}
