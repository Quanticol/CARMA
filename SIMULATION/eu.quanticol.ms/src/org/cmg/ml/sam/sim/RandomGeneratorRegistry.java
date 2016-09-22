/**
 * 
 */
package org.cmg.ml.sam.sim;

import java.util.Collection;
import java.util.HashMap;

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
		RandomGenerator rg = getInstance().get();
		int idx = rg.nextInt(collection.size());
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
	
	public static double rnd() {
		RandomGenerator rg = getInstance().get();
		return rg.nextDouble();
	}
	
}
