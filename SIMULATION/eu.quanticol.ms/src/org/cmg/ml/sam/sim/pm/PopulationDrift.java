/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

/**
 * @author loreti
 *
 */
public class PopulationDrift<S> {
	
	private HashMap<S,Drift> populationDrift;
	
	public PopulationDrift() {
		this.populationDrift = new HashMap<>();
	}
	
	public void addToPreset( S s , int x ) {
		Drift d = getDrift( s );
		d.addToPreset(x);
	}

	public void addToPoset( S s , int x ) {
		Drift d = getDrift( s );
		d.addToPoset(x);
	}

	private Drift getDrift(S s) {
		Drift d = populationDrift.get(s);
		if (d == null) {
			d = new Drift();
			populationDrift.put(s, d);
		}
		return d;
	}
	
	public Set<Entry<S, Drift>> getDriftMap() {
		return populationDrift.entrySet();
	}
	
	public PopulationState<S> apply( PopulationState<S> state ) {
		PopulationState<S> newState = state.copy();
		populationDrift.forEach((s,d) -> newState.apply(s,d));
		return newState;
	}
}



