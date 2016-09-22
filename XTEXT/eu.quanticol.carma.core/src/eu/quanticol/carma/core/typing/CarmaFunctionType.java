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
public class CarmaFunctionType extends CarmaType {

	private CarmaType argument;
	private CarmaType result;

	public CarmaFunctionType(CarmaType argument, CarmaType result) {
		super( CarmaType.TypeCode.FUNCTION );
		this.argument = argument;
		this.result = result;
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		if (obj instanceof CarmaFunctionType) {
			CarmaFunctionType other = (CarmaFunctionType) obj;
			return this.argument.equals(other.argument)&&this.result.equals(other.result);
		}
		return false;
	}

	@Override
	protected String doToString() {
		return argument.toString()+"->"+result.toString();
	}
	
	public CarmaType getArguments() {
		return argument;
	}
	
	public CarmaType getResult() {
		return result;
	}


	
}
