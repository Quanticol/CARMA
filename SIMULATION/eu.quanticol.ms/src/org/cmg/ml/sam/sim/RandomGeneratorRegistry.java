/**
 * 
 */
package org.cmg.ml.sam.sim;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.function.Function;

import org.apache.commons.math3.random.RandomGenerator;

/**
 * @author loreti
 *
 */
public class RandomGeneratorRegistry {
	
	private static RandomGeneratorRegistry instance;
	
	private RandomGenerator rg;
	
	private HashMap<Thread, RandomGenerator> registry;
	
	private RandomGeneratorRegistry() {
		this.registry = new HashMap<>();
		this.rg = new DefaultRandomGenerator();
	}
	
	public synchronized static RandomGeneratorRegistry getInstance() {
		if (instance == null) {
			instance = new RandomGeneratorRegistry();
		}
		return instance;
	}
	
	public synchronized void register( RandomGenerator rg ) {
		registry.put(Thread.currentThread(), rg);
	}
	
	public synchronized void unregister() {
		registry.remove(Thread.currentThread());
	}
	
	public synchronized RandomGenerator get( ) {
		return retrieve( Thread.currentThread() );
	}

	private RandomGenerator retrieve(Thread currentThread) {
		RandomGenerator rg = registry.get(currentThread);
		if (rg == null) {
			rg = this.rg;			
		}
		return rg;
	}

	@SafeVarargs 
	public static <T> T uniform( T ... data ) {
		RandomGenerator rg = getInstance().get();
		return data[rg.nextInt(data.length)];
	}
	
	public static <T> T uniformSelect( Collection<T> collection ) {
		if (collection.size()==0) {
			System.out.println("IS EMPTY!!!");
			return null;
		}
		int idx = 0;
		if (collection.size()>1) {
			RandomGenerator rg = getInstance().get();
			idx = rg.nextInt(collection.size());
		}
		int counter = 0;
		T last = null;
		for (T t : collection) {
			last = t;
			if (counter == idx) {
				return t;
			} else {
				counter++;
			}
		}
		return last;
	}

	public static <T> T select( Collection<T> collection , Function<T,Double> weight ) {
		if (collection.size()==0) {
			System.out.println("IS EMPTY!!!");
			return null;
		}
		double[] weightArray = new double[collection.size()];
		double total = 0.0;
		ArrayList<T> elements = new ArrayList<>();
		int counter = 0;
		for (T e : collection) {
			Double w = weight.apply(e);
			if (w == null) {
				w = 0.0;
			}
			total += w;
			weightArray[counter] = total;
			elements.add(e);
			counter++;

		}		
		return select( elements , weightArray , total );
	}

	private static <T> T select(ArrayList<T> elements, double[] weightArray, double total) {
		if (total == 0) {
			return null;
		}
		double val = total*rnd();
		for (int i=0 ; i<weightArray.length; i++ ) {
			if (val<weightArray[i]) {
				return elements.get(i);
			}
		}
		return null;
	}

	public static double rnd() {
		RandomGenerator rg = getInstance().get();
		return rg.nextDouble();
	}
	
	public static double normal(double mean, double sd) {
		RandomGenerator rg = getInstance().get();
		return rg.nextGaussian()*sd+mean;
	}
}
