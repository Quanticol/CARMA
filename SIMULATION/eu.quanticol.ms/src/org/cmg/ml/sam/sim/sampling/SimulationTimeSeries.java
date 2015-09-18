/**
 * 
 */
package org.cmg.ml.sam.sim.sampling;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

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
	
	public void printTimeSeries( PrintStream out ) {
		out.println(name);
		for( int i=0 ; i<data.length ; i++ ) {
			out.println(getTime(i)+"\t"+getMean(i));
		}
	}
	
	public void saveTo( String path ) throws FileNotFoundException {
		File output = new File( path+"/"+name+".dat");
		PrintStream ps = new PrintStream(output);
		printTimeSeries(ps);
		ps.close();
	}
	
}
