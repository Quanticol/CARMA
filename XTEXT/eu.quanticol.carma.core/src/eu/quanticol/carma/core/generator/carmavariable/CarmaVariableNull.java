package eu.quanticol.carma.core.generator.carmavariable;

public class CarmaVariableNull implements CarmaVariableAssignment {

	private String value = "null CarmaVariableAssignment";
	
	public CarmaVariableNull(){
	}

	@Override
	public Object getValue() {
		return (Object) value;
	}

	@Override
	public boolean setValue(Object obj) {
		return true;
	}
	
	
	
}
