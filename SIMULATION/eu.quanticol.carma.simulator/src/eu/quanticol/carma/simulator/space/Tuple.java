package eu.quanticol.carma.simulator.space;

import java.util.Arrays;

public class Tuple {
	
	private Object[] values;
	
	public Tuple( Object ... values ) {
		this.values = values;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(values);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Tuple other = (Tuple) obj;
		if (!Arrays.deepEquals(values, other.values))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return Arrays.deepToString(values);
	}

	public Object get(int i) {
		return values[i];
	}
	
	public <T> T get(int i, Class<T> clazz ) {
		return clazz.cast(get(i));
	}

		
}