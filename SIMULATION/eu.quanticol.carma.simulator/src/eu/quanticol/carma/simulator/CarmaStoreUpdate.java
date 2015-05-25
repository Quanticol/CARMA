/**
 * 
 */
package eu.quanticol.carma.simulator;

import org.apache.commons.math3.random.RandomGenerator;

/**
 * @author loreti
 *
 */
public interface CarmaStoreUpdate {

	CarmaStoreUpdate NO_UPDATE = new CarmaStoreUpdate() {		
		@Override
		public void update(RandomGenerator r, CarmaStore store) {
		}
	};

	public void update( RandomGenerator r , CarmaStore store );
	
}
