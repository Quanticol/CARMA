package eu.quanticol.carma.core.generator.carmaVariable;

import eu.quanticol.carma.core.carma.VariableReference;

public class CarmaVariableReference implements CarmaVariableAssignment {

	private VariableReference value = null;
	
	public CarmaVariableReference(){
	}

	@Override
	public Object getValue() {
		return (Object) value;
	}
	
	@Override
	public boolean setValue(Object obj) {
		if(obj instanceof VariableReference){
			this.value = (VariableReference) obj;
			return true;
		} else {
			return false;
		}
	}
	
	
	
}