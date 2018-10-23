/**
 * 
 */
package eu.quanticol.carma.tools.monitoring.jsstl;

/**
 * @author loreti
 *
 */
public class SignalPoint<D> {

	private final double time;
	
	private final D value;
	
	public SignalPoint( double time, D value ) {
		if (value == null) {
			throw new NullPointerException();
		}
		this.time = time;
		this.value = value;
	}

	/**
	 * @return the time
	 */
	public double getTime() {
		return time;
	}

	/**
	 * @return the value
	 */
	public D getValue() {
		return value;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return (int) (time*1000);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof SignalPoint<?>) {
			SignalPoint<?> other = (SignalPoint<?>) obj;
			return (this.time==other.time)&&(this.value.equals(other.value));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return time+":"+value.toString();
	}
	
	
}
