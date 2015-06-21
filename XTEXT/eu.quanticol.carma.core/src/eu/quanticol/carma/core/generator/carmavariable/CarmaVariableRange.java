package eu.quanticol.carma.core.generator.carmavariable;

import java.util.ArrayList;

public class CarmaVariableRange implements CarmaVariableAssignment {
	
	private ArrayList<Integer> value;
	
	public CarmaVariableRange(){
		
	}

	@Override
	public Object getValue() {
		return (Object) value;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean setValue(Object obj) {
		if(obj instanceof ArrayList<?>){
			this.value = (ArrayList<Integer>) obj;
			return true;
		} else {
			return false;
		}
	}

}
