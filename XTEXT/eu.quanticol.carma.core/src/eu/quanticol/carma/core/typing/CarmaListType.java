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
public class CarmaListType extends CarmaType {
	

	private CarmaType type;

	public CarmaListType(CarmaType type) {
		super( CarmaType.TypeCode.LIST );
		this.type = type;
	}

	@Override
	protected boolean doEquals(CarmaType obj) {
		if (obj instanceof CarmaListType) {
			CarmaListType other = (CarmaListType) obj;
			return ((this.type==null)&&(other.type==null))||
					((this.type!=null)&&this.type.equals(other.type));
		}
		return false;
	}

	public CarmaType getElementsType() {
		return type;
	}

	@Override
	protected String doToString() {
		return super.doToString()+"<"+(type!=null?type.toString():"?")+">";
	}

	
}
