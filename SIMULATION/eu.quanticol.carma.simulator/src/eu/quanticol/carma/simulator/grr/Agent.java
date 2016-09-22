/**
 * 
 */
package eu.quanticol.carma.simulator.grr;

/**
 * @author loreti
 *
 */
public class Agent {

	private final int id;
	
	private final String name;
	
	public Agent( int id , String name ) {
		this.name = name;
		this.id = id;
	}


	@Override
	public int hashCode() {
		return id;
		//return name.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Agent) {
			return (this==obj)||((Agent) obj).name.equals(this.name);
		}
		return false;
	}

	@Override
	public String toString() {
		return name;
	}


}
