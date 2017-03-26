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
import java.util.LinkedList;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.sampling.SamplingFunction;
import org.cmg.ml.sam.sim.sampling.SimulationTimeSeries;
import org.cmg.ml.sam.sim.util.WeightedElement;
import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * @author loreti
 *
 */
public class SimulationEnvironment<S extends ModelI> {

	protected RandomGenerator random;
	private SimulationFactory<S> factory;
	private S model;
	private SamplingFunction<S> sampling_function;
	private int iterations = 0;

	public SimulationEnvironment(SimulationFactory<S> factory) {
//		if (factory == null) {
//			throw new NullPointerException();
//		}
		this.factory = factory;
		this.random = new DefaultRandomGenerator();
	}

	public SimulationEnvironment() {
		this(null);
	}
	

	public void setFactory(SimulationFactory<S> factory) {
		this.factory = factory;
	}
	
	public void seed(long seed) {
		random.setSeed(seed);
	}

	public void setSampling(SamplingFunction<S> sampling_function) {
		this.sampling_function = sampling_function;
	}

	public synchronized void simulate(SimulationMonitor monitor , int iterations, double deadline) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();
		rgi.register(random);
		for (int i = 0; (((monitor == null)||(!monitor.isCancelled()))&&(i < iterations)) ; i++) {
			if (monitor != null) {
				monitor.startIteration( i );
			}
			System.out.print('<');
			if ((i + 1) % 50 == 0) {
				System.out.print(i + 1);
			}
			System.out.flush();
			doSimulate(factory.getModel(),monitor,deadline);
			if (monitor != null) {
				monitor.endSimulation( i );
			}
			System.out.print('>');
			if ((i + 1) % 50 == 0) {
				System.out.print("\n");
			}
			System.out.flush();
			this.iterations++;
		}
		rgi.unregister();		
	}
	
	public synchronized void simulate(int iterations, double deadline) {
		simulate( null , iterations , deadline );
	}

	public void simulate( S model , SamplingFunction<S> measure , double deadline) {
		this.sampling_function = measure;
		doSimulate(model,null,deadline);
	}
	
	public synchronized double simulate(double deadline) {
		RandomGeneratorRegistry rgi = RandomGeneratorRegistry.getInstance();
		rgi.register(random);
		double result = doSimulate(deadline);
		rgi.unregister();
		return result;
	}

	private double doSimulate(S model, SimulationMonitor monitor , double deadline) {
		this.model = model;
		double time = 0.0;
		if (sampling_function != null) {
			sampling_function.start();
			sampling_function.sample(time, model);
		}
		while (((monitor == null)||(!monitor.isCancelled()))&&(time < deadline)) {
			double dt = doAStep();
			if (dt <= 0) {
				if (sampling_function != null) {
					sampling_function.end(time);
				}
				return time;
			}
			time += dt;
			this.model.timeStep(dt);
			if (monitor != null && !monitor.isCancelled()) {
				monitor.update(time);
			}
			if (sampling_function != null) {
				sampling_function.sample(time, model);
			}
		}
		
		if (sampling_function != null) {
			sampling_function.end(time);
		}
		return time;	
	}
	private double doSimulate(double deadline) {
		return doSimulate(factory.getModel(),null,deadline);
	}

	private double doAStep() {
		WeightedStructure<Activity> agents = this.model.getActivities( random );
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

	public LinkedList<SimulationTimeSeries> getTimeSeries( ) {
		if (sampling_function == null) {
			return null;
		}
		return sampling_function.getSimulationTimeSeries( iterations );
	}
}
