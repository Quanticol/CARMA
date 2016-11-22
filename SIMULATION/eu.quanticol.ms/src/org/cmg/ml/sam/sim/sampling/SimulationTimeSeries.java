/**
 * 
 */
package org.cmg.ml.sam.sim.sampling;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;

import org.apache.commons.math3.stat.descriptive.SummaryStatistics;

/**
 * @author loreti
 *
 */
public class SimulationTimeSeries {

	private SummaryStatistics[] data;
	private double dt;
	private String name;
	private int replications;
	
	
	public SimulationTimeSeries( String name , double dt , int replications , SummaryStatistics[] data ) {
		this.name = name;
		this.dt = dt;
		this.data = data;
		this.replications = replications;
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
	
	public int getSize() {
		return data.length;
	}
	
	public double getConfidenceInterval( int i ) {
		if (replications<=0) {
			return 0.0;
		} else {
			return this.getStandardDeviation(i)/Math.sqrt( replications );		
		}
	}

	
	public void writeToCSV( StringWriter writer ) {
		for( int i=0 ; i<data.length ; i++ ) {
			writer.write(getTime(i)+";"+getMean(i)+";"+getStandardDeviation(i)+";"+getConfidenceInterval(i)+";\n");
			writer.flush();
		}
	}

	public void writeToCSV( PrintWriter writer ) {
		for( int i=0 ; i<data.length ; i++ ) {
			writer.println(getTime(i)+";"+getMean(i)+";"+getStandardDeviation(i)+";"+getConfidenceInterval(i)+";");
			writer.flush();
		}
	}
}
