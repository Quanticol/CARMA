package eu.quanticol.carma.core.generator.carmavariable;

import eu.quanticol.carma.core.carma.VariableReference;

public class CarmaVariableReference implements CarmaVariableAssignment {

	private String value = "";
	
	public CarmaVariableReference(){
	}

	@Override
	public Object getValue() {
		return (Object) value;
	}
	
	@Override
	public boolean setValue(Object obj) {
		if(obj instanceof String){
			this.value = (String) obj;
			return true;
		} else {
			return false;
		}
	}
	
	
	
}