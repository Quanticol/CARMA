/**
 * 
 */
package eu.quanticol.carma.simulator.grr;


/**
 * @author loreti
 *
 */
public class Instance {

	private final Agent agent;
	
	private final Configuration configuration;
	
	public Instance( Agent species , Configuration configuration ) {
		this.agent = species;
		this.configuration = configuration;
	}



	@Override
	public String toString() {
		return agent.toString()+'['+configuration.toString()+']';
	}



	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((configuration == null) ? 0 : configuration.hashCode());
		result = prime * result + ((agent == null) ? 0 : agent.hashCode());
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
		if (!(obj instanceof Instance)) {
			return false;
		}
		Instance other = (Instance) obj;
		if (configuration == null) {
			if (other.configuration != null) {
				return false;
			}
		} else if (!configuration.equals(other.configuration)) {
			return false;
		}
		if (agent == null) {
			if (other.agent != null) {
				return false;
			}
		} else if (!agent.equals(other.agent)) {
			return false;
		}
		return true;
	}



	public Agent getSpecie() {
		return agent;
	}



	public Configuration getConfiguration() {
		return configuration;
	}

	
	
}
