/**
 * 
 */
package eu.quanticol.carma.core.typing;

import java.util.Arrays;

import org.eclipse.emf.ecore.EObject;

import eu.quanticol.carma.core.carma.EnumDefinition;
import eu.quanticol.carma.core.carma.RecordDefinition;
import eu.quanticol.carma.core.utils.Util;

/**
 * @author loreti
 *
 */
public class CarmaTupleType extends CarmaType {
	

	private CarmaType[] types;

	protected CarmaTupleType(CarmaType ... types ) {
		super(CarmaType.TypeCode.TUPLE);
		this.types = types;
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		if (obj instanceof CarmaTupleType) {
			return Arrays.deepEquals(types, ((CarmaTupleType) obj).types);
		}
		return false;
	}

	@Override
	public String doToString() {
		return Arrays.toString(types);
	}
	
	public CarmaType[] getTypes() {
		return types;
	}


	
}
