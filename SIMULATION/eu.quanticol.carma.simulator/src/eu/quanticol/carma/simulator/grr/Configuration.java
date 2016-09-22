/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

import java.util.Arrays;
import java.util.function.Function;


/**
 * @author loreti
 *
 */
public class Configuration {
	
	private Object[] data;
	
	public Configuration( int size ) {
		this.data = new Object[size];
	}
	
	public Configuration( Object ... data ) {
		this.data = data;
	}
	
	public <T> T get( Class<T> clazz , int idx ) {
		return clazz.cast(data[idx]);
	}

	public Configuration copy() {
		return new Configuration( Arrays.copyOf(data, data.length) );
	}
	
	public boolean satisfy( Function<Configuration,Boolean> predicate ) {
		try {
			return predicate.apply(this);
		} catch ( Exception e ) {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return Arrays.hashCode(data);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Configuration) {
			return (this==obj)||(Arrays.deepEquals(this.data, ((Configuration) obj).data));
		}
		return false;
	}

	@Override
	public String toString() {
		return Arrays.deepToString(data);
	}
}
