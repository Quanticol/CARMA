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
public interface SamplingFunction<S> {

	public void sample(double time, S context);

	public void end(double time);

	public void start();

	public LinkedList<SimulationTimeSeries> getSimulationTimeSeries( int replications );
}
