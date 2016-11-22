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

import java.util.LinkedList;
import java.util.List;

/**
 * @author loreti
 *
 */
public class WeightedLinkedList<S> implements WeightedStructure<S> {

	private double totalWeight;

	private LinkedList<WeightedElement<S>> list;

	public WeightedLinkedList() {
		this.totalWeight = 0.0;
		this.list = new LinkedList<WeightedElement<S>>();
	}

	@Override
	public double getTotalWeight() {
		return this.totalWeight;
	}

	@Override
	public WeightedElement<S> select(double w) {
		double total = 0.0;
		for (WeightedElement<S> weightedElement : list) {
			total += weightedElement.getWeight();
			if (w <= total) {
				return weightedElement.residual(w);
			}
		}
		return null;
	}

	@Override
	public WeightedStructure<S> add(double w, S s) {
		totalWeight += w;
		list.add(new WeightedElement<S>(w, s));
		return this;
	}

	public WeightedStructure<S> add(WeightedElement<S> we) {
		totalWeight += we.getWeight();
		list.add(we);
		return this;
	}

	@Override
	public WeightedStructure<S> add(WeightedStructure<S> s) {
		if (s.getTotalWeight() == 0.0) {
			return this;
		}
		return new ComposedWeightedStructure<S>(this, s);
	}

	@Override
	public List<WeightedElement<S>> getAll() {
		return list;
	}

}
