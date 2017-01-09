/**
 * 
 */
package eu.quanticol.carma.simulator2;

import java.util.function.Function;

/**
 * @author loreti
 *
 */
public class Component {
	
	private final Configuration configuration;
	
	private final Store store;
	
	public Component( Configuration configuration , Store store ) {
		this.configuration = configuration;
		this.store = store;
	}
	
	public Store getStore() {
		return store;
	}
	
	public Configuration getConfiguration() {
		return configuration;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((configuration == null) ? 0 : configuration.hashCode());
		result = prime * result + ((store == null) ? 0 : store.hashCode());
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
		if (!(obj instanceof Component)) {
			return false;
		}
		Component other = (Component) obj;
		if (configuration == null) {
			if (other.configuration != null) {
				return false;
			}
		} else if (!configuration.equals(other.configuration)) {
			return false;
		}
		if (store == null) {
			if (other.store != null) {
				return false;
			}
		} else if (!store.equals(other.store)) {
			return false;
		}
		return true;
	}
	
	public Component apply( Function<Configuration, Configuration> step , Function<Store, Store> update ) {
		return new Component(step.apply(configuration), update.apply(store));
	}

}
