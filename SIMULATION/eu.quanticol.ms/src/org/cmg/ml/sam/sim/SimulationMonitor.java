/**
 * 
 */
package org.cmg.ml.sam.sim;

/**
 * @author loreti
 *
 */
public interface SimulationMonitor {

	void startIteration(int i);

	void endSimulation(int i);

	boolean isCancelled();
	
	default void update(double time) { };
}
