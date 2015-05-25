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
package org.cmg.ml.sam.sim;

import java.util.ArrayList;
import java.util.Random;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.sampling.SamplingFunction;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public class SimulationEnvironment<S extends ModelI> {

	protected ArrayList<Agent<S>> agents;
	protected RandomGenerator random;
	private SimulationFactory<S> factory;
	private S model;
	private SamplingFunction<S> sampling_function;

	public SimulationEnvironment(SimulationFactory<S> factory) {
		if (factory == null) {
			throw new NullPointerException();
		}
		this.factory = factory;
		this.random = new DefaultRandomGenerator();
	}

	public void seed(long seed) {
		random.setSeed(seed);
	}

	public void setSampling(SamplingFunction<S> sampling_function) {
		this.sampling_function = sampling_function;
	}

	public synchronized void simulate(int iterations, double deadline) {
		for (int i = 0; i < iterations; i++) {
			System.out.print('<');
			if ((i + 1) % 50 == 0) {
				System.out.print(i + 1);
			}
			System.out.flush();
			simulate(deadline);
			System.out.print('>');
			if ((i + 1) % 50 == 0) {
				System.out.print("\n");
			}
			System.out.flush();
		}
	}

	public synchronized double simulate(double deadline) {
		this.model = this.factory.getModel();
		double time = 0.0;
		if (sampling_function != null) {
			sampling_function.start();
			sampling_function.sample(time, model);
		}
		while (time < deadline) {
			double dt = doAStep();
			if (dt <= 0) {
				if (sampling_function != null) {
					sampling_function.end(time);
				}
				return time;
			}
			time += dt;
			this.model.timeStep(dt);
			if (sampling_function != null) {
				sampling_function.sample(time, model);
			}
		}
		if (sampling_function != null) {
			sampling_function.end(time);
		}
		return time;
	}

	private double doAStep() {
		WeightedStructure<Activity> agents = this.model.getActivities();
		double totalRate = agents.getTotalWeight();
		if (totalRate == 0.0) {
			return 0.0;
		}
		double dt = (1.0 / totalRate) * Math.log(1 / (random.nextDouble()));
		double select = random.nextDouble() * totalRate;
		WeightedElement<Activity> wa = agents.select(select);
		if (wa == null) {
			return 0.0;
		}
		wa.getElement().execute(random);
		return dt;
	}

	public double nextDouble() {
		return random.nextDouble();
	}

	public int nextInt(int zones) {
		return random.nextInt(zones);
	}

}
