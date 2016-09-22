/*******************************************************************************
 * Copyright (c) 2015 QUANTICOL EU Project.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 *
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Michele Loreti (University of Firenze) - initial API and implementation
 *******************************************************************************/
package org.cmg.ml.sam.sim.sampling;

import java.io.PrintStream;
import java.util.LinkedList;

import org.apache.commons.math3.stat.descriptive.SummaryStatistics;

/**
 * @author loreti
 *
 */
public class StatisticSampling<S> implements SamplingFunction<S> {

	private SummaryStatistics[] data;
	private Measure<S> measure;
	private double last_measure;
	private double dt;
	private double next_time;
	private int current_index;
	private double new_measure;

	public StatisticSampling(int samples, double dt, Measure<S> measure) {
		this.data = new SummaryStatistics[samples];
		this.measure = measure;
		this.dt = dt;
		init();
	}

	private void init() {
		for (int i = 0; i < data.length; i++) {
			data[i] = new SummaryStatistics();
		}
	}

	@Override
	public void sample(double time, S context) {
		this.new_measure = measure.measure(context);
		if ((time >= this.next_time) && (this.current_index < this.data.length)) {
			recordMeasure(time);
		} else {
			this.last_measure = this.new_measure;
		}
	}

	private void recordMeasure(double time) {
		while ((this.next_time<time)&&(this.current_index<this.data.length)) {
			this.recordSample();
		} 
		this.last_measure = this.new_measure;		
		if (this.next_time == time) {
			this.recordSample();
		}
	}
	
	private void recordSample() {
		this.data[this.current_index].addValue(this.last_measure);
		this.current_index++;
		this.next_time += this.dt;
	}
	

	@Override
	public void end(double time) {
		while (this.current_index < this.data.length) {
			this.data[this.current_index].addValue(this.last_measure);
			this.current_index++;
			this.next_time += this.dt;
		}
	}

	@Override
	public void start() {
		this.current_index = 0;
		this.next_time = 0;
	}

	public void printTimeSeries(PrintStream out) {
		double time = 0.0;
		for (int i = 0; i < this.data.length; i++) {
			out.println(time + "\t" + this.data[i].getMean());
			time += dt;
		}
	}
	
	public void printName(PrintStream out){
		out.print(this.measure.getName());
	}
	
	public void printlnName(PrintStream out){
		out.println(this.measure.getName());
	}

	@Override
	public LinkedList<SimulationTimeSeries> getSimulationTimeSeries( int replications ) {
		SimulationTimeSeries stt = new SimulationTimeSeries(measure.getName(), dt, replications, data);
		LinkedList<SimulationTimeSeries> toReturn = new LinkedList<>();
		toReturn.add(stt);
		return toReturn;
	}

	public int getSize() {
		return data.length;
	}

}
