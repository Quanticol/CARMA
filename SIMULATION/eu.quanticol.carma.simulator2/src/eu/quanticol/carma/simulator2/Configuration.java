/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.Arrays;

/**
 * @author loreti
 *
 */
public class Configuration {
	
	private int componentId;
	
	private int[] agentsId;	
	
	public Configuration( int componentId , int[] agentsId ) {
		this.componentId = componentId;
		this.agentsId = agentsId;
	}
	
	public int getComponentId() {
		return componentId;
	}
	
	public int getAgentId( int i ) {
		return agentsId[i];
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(agentsId);
		result = prime * result + componentId;
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
		if (!(obj instanceof Configuration)) {
			return false;
		}
		Configuration other = (Configuration) obj;
		if (!Arrays.equals(agentsId, other.agentsId)) {
			return false;
		}
		if (componentId != other.componentId) {
			return false;
		}
		return true;
	}
	
	public Configuration set( int i , int x ) {
		int[] copy = Arrays.copyOf(agentsId, agentsId.length);
		copy[i] = x;
		return new Configuration(componentId, copy);
	}
}
