/**
 * 
 */
package org.cmg.ml.sam.sim.sampling;

import org.apache.commons.math3.stat.descriptive.SummaryStatistics;

/**
 * @author loreti
 *
 */
public class SimulationTimeSeries {

	private SummaryStatistics[] data;
	private double dt;
	private String name;
	
	
	public SimulationTimeSeries( String name , double dt , SummaryStatistics[] data ) {
		this.name = name;
		this.dt = dt;
		this.data = data;
	}
	
	public String getName() {
		return name;
	}
	
	public double getMean( int i ) {
		return data[i].getMean();
	}
	
	public double getStandardDeviation( int i ) {
		return data[i].getStandardDeviation();
	}
	
	public double getTime( int i ) {
		return i*dt;
	}	
	
	public double getMean( double t ) {
		int i = (int) (t/dt);
		if (i>=data.length) {
			i = data.length-1;
		}
		return getMean( i );
	}
	
}