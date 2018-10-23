/**
 * 
 */
package org.cmg.ml.sam.sim.pm;

import java.util.Arrays;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The instances of this class represent a generic population state having species of type <code>S</code>. 
 * 
 * 
 * @author loreti
 *
 */
public class PopulationState {

	/**
	 * Internal representation of the state as continuing vector.
	 */
	private final int[] populationVector;
	
	public PopulationState( int size ) {
		this(new int[size]);
	}
	
	public PopulationState(int[] state) {
		this.populationVector = state;
	}

	public double getOccupancy( int i ) {
		try {
			return populationVector[i];
		} catch (ArrayIndexOutOfBoundsException e) {
			return 0;
		}
	}
	
	public PopulationState apply( Update update ) {
		int[] newState = Arrays.copyOf(populationVector, populationVector.length);
		for (Entry<Integer, Integer> u : update.getUpdate()) {
			int idx = u.getKey();
			int newValue = newState[idx]+u.getValue(); 
			if (newValue>=0) {
				newState[idx] = newValue;
			} else {
				throw new IllegalArgumentException("Population Vector: "+this+" newState: "+Arrays.toString(newState)+" Update: "+update+" idx: "+idx+" newValue: "+newValue+" u: "+u);
			}
		}		
		return new PopulationState(newState);
	}
	
	public double min( Function<Integer, Double> f ) {
		return min( i -> true , f );
	}
	
	public double min( Predicate<Integer> p , Function<Integer, Double> f) {
		double min = Double.MAX_VALUE;
		for( int i=0 ; i<populationVector.length ; i++ ) {
			if ((p.test(i))&&(this.populationVector[i]>0)) {
				min = Math.min(min,f.apply(i));
			}
		}
		return min;
	}
	
	public double max( Function<Integer, Double> f ) {
		return max( i -> true , f );
	}
	
	public double max( Predicate<Integer> p , Function<Integer, Double> f) {
		double max = Double.MIN_VALUE;
		for( int i=0 ; i<populationVector.length ; i++ ) {
			if ((p.test(i))&&(this.populationVector[i]>0)) {
				max = Math.max(max,f.apply(i));
			}
		}
		return max;
	}
	
	public double average( Predicate<Integer> p , Function<Integer, Double> f ) {
		double total = 0.0;
		int counter = 0;
		for( int i=0 ; i<populationVector.length ; i++ ) {
			if (p.test(i)&&(populationVector[i]>0)) {
				counter += populationVector[i];
				total += populationVector[i]*f.apply(i);
			}
		}
		return total/counter;
	}
	
	public double average( Function<Integer, Double> f ) {
		return average( s -> true , f );
	}
	
	public int count( Set<Integer> species ) {
		int result = 0;
		for (Integer i : species) {
			result += this.populationVector[i];
		}
		return result;
	}
	
	public int count( Predicate<Integer> p ) {
		int result = 0;
		for( int i=0 ; i< this.populationVector.length ; i++ ) {
			result += this.populationVector[i];
		}
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return Arrays.toString(populationVector);
	}

	public int size() {
		return populationVector.length;
	}

}
