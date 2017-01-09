/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.Arrays;

/**
 * @author loreti
 *
 */
public class Store {

	private Object[] data;
	
	public Store( Object ... data ) {
		this.data = data;
	}
	
	public <T> T get( int idx , Class<T> clazz ) {
		return clazz.cast(data[idx]);
	}
	
	public void set( int idx , Object value ) {
		this.data[idx] = value;
	}
	
	public Store copy() {
		return new Store( Arrays.copyOf(data, data.length) );
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(data);
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof Store)) {
			return false;
		}
		Store other = (Store) obj;
		if (!Arrays.equals(data, other.data)) {
			return false;
		}
		return true;
	}
	
}
