package eu.quanticol.carma.core.generator.carmavariable;

import eu.quanticol.carma.core.carma.MethodExpressions;

public class CarmaVariableMethodRefence implements CarmaVariableAssignment {
	
	private MethodExpressions value = null;
	
	public CarmaVariableMethodRefence(){
		
	}

	@Override
	public Object getValue() {
		return (Object) value;
	}

	@Override
	public boolean setValue(Object obj) {
		if(obj instanceof MethodExpressions){
			this.value = (MethodExpressions) obj;
			return true;
		} else {
			return false;
		}
			
	}

}
