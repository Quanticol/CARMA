package eu.quanticol.carma.core.generator.carmavariable;

public class CarmaVariableInteger implements CarmaVariableAssignment {

	private Integer value = 0;
	
	public CarmaVariableInteger(){
	}

	@Override
	public Object getValue() {
		return (Object) value;
	}

	@Override
	public boolean setValue(Object obj) {
		if(obj instanceof Integer){
			this.value = (Integer) obj;
			return true;
		} else {
			return false;
		}
	}
	
	
	
}
