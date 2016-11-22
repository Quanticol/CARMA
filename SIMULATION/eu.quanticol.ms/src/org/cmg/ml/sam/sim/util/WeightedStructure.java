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
/**
 * 
 */
package org.cmg.ml.sam.sim.util;

import java.util.List;

/**
 * @author loreti
 *
 */
public interface WeightedStructure<S> {

	public double getTotalWeight();

	public WeightedElement<S> select(double w);

	public WeightedStructure<S> add(double w, S s);

	public WeightedStructure<S> add(WeightedStructure<S> s);

	public List<WeightedElement<S>> getAll();
	
}
