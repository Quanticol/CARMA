/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * The instances of this class represent a generic population state having species of type <code>S</code>. 
 * 
 * 
 * @author loreti
 *
 */
public class PopulationState<S> {

	/**
	 * Internally the state is represented via a mapping from <code>S</code> to <code>Integer</code>.
	 */
	private final HashMap<S,Integer> population;

	/**
	 * Creates a new population state with a given population. 
	 * 
	 * @param population
	 */
	public PopulationState( HashMap<S,Integer> population ) {
		this.population = population;
	}
	
	/**
	 * Creates an empty population state.
	 */
	public PopulationState() {
		this( new HashMap<>() );
	}
	
	protected Set<Map.Entry<S, Integer>> getEntries() {
		return population.entrySet();
	}
	
	public int getOccupancy( S s ) {
		return population.getOrDefault(s, 0);
	}
	
	public void set( S s , Integer value ) {
		population.put(s, value);		
	}
	
	public void apply( S s , Drift d ) {
		int value = population.getOrDefault(s, 0);
		population.put(s, value-d.getPreset()+d.getPoset());
	}
	
	@SuppressWarnings("unchecked")
	public PopulationState<S> copy() {
		return new PopulationState<>( (HashMap<S, Integer>) population.clone() );
	}
	

	public double min( Function<S, Double> f ) {
		return min( s -> true , f );
	}
	
	public double min( Predicate<S> p , Function<S, Double> f) {
		double min = Double.MAX_VALUE;
		for (Entry<S, Integer> entry : getEntries()) {
			S s = entry.getKey();
			if (p.test(s)&&entry.getValue()>0) {
				min = Math.min(min, f.apply(s));
			}
		}		
		return min;
	}
	
	public double max( Function<S, Double> f ) {
		return max( s -> true , f );
	}
	
	public double max( Predicate<S> p , Function<S, Double> f) {
		double max = Double.MIN_VALUE;
		for (Entry<S, Integer> entry : getEntries()) {
			S s = entry.getKey();
			if (p.test(s)&&entry.getValue()>0) {
				max = Math.max(max, f.apply(s));
			}
		}		
		return max;
	}
	
	public double average( Predicate<S> p , Function<S, Double> f ) {
		double total = 0.0;
		int counter = 0;
		for (Entry<S, Integer> entry : getEntries()) {
			S s = entry.getKey();
			int value = entry.getValue();
			if (p.test(s)&&value>0) {
				total += entry.getValue()*f.apply(s);
				counter += value;
			}
		}		
		return total/counter;
	}
	
	public double average( Function<S, Double> f ) {
		return average( s -> true , f );
	}
	
	public Stream<Entry<S, Integer>> select( Predicate<S> p ) {
		return getEntries().stream().filter(e -> (e.getValue()>0)&&(p.test(e.getKey())));
	}
}
