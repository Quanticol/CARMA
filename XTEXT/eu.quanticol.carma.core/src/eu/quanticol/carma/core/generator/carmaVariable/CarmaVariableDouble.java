package eu.quanticol.carma.core.generator.carmaVariable;

public class CarmaVariableDouble implements CarmaVariableAssignment {

	private Double value = 0.0;
	
	public CarmaVariableDouble(){
	}

	@Override
	public Object getValue() {
		return (Object) value;
	}
	
	@Override
	public boolean setValue(Object obj) {
		if(obj instanceof Double){
			this.value = (Double) obj;
			return true;
		} else {
			return false;
		}
	}
	
	
	
}