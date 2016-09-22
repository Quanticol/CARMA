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

import java.util.LinkedList;

/**
 * @author loreti
 *
 */
public class SamplingLog<S> implements SamplingFunction<S> {

	private double dt;
	private double last_time = 0.0;

	public SamplingLog(double dt) {
		this.dt = dt;
	}

	@Override
	public void sample(double time, S context) {
		while (time >= last_time) {
			System.out.println(last_time + ": " + context.toString());
			this.last_time += dt;
		}
	}

	@Override
	public void end(double time) {
		System.out.println(time + ": END");
	}

	@Override
	public void start() {
		this.last_time = 0.0;
	}

	@Override
	public LinkedList<SimulationTimeSeries> getSimulationTimeSeries( int replications ) {
		return new LinkedList<>();
	}

}
