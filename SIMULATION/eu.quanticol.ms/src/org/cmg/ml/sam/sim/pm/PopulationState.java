/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.Map;
import java.util.Map.Entry;

/**
 * @author loreti
 *
 */
public abstract class PopulationState<S> {

	
	
	public PopulationState( ) {
	}
	
	protected abstract Iterable<Map.Entry<S, Integer>> getEntries();
	
	public abstract int getOccupancy( S s );
	
	protected abstract void set( S s , Integer value );
	
	protected abstract PopulationState<S> copy();
	
	public boolean contains( PopulationState<S> state ) {
		for (Entry<S,Integer> entry : state.getEntries()) {
			if (getOccupancy(entry.getKey())<entry.getValue()) {
				return false;
			}
		}
		return true;
	}

	private PopulationState<S> update( int df , PopulationState<S> state ) {
		PopulationState<S> copy = this.copy();
		for (Entry<S,Integer> entry : state.getEntries()) {
			S s = entry.getKey();
			Integer value = copy.getOccupancy(s);
			Integer remove = entry.getValue();
			int newValue = value+(df*remove);
			if (newValue < 0) {
				return null;
			} else {
				copy.set(s, newValue);
			}
		}
		return copy;
	}
	
	public PopulationState<S> add( PopulationState<S> state ) {
		return this.update(1, state);
	}

	public PopulationState<S> remove( PopulationState<S> state ) {
		return this.update(-1, state);
	}

	
}
