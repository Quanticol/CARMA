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
public class ComposedWeightedStructure<S> implements WeightedStructure<S> {

	@Override
	public String toString() {
		if (total_weight == 0) {
			return "0";
		}
		return left+" - "+right;
	}

	private double total_weight;
	private WeightedStructure<S> left;
	private WeightedStructure<S> right;

	public ComposedWeightedStructure() {
		this(null, null);
	}

	public ComposedWeightedStructure(WeightedStructure<S> left,
			WeightedStructure<S> right) {
		total_weight = 0.0;
		this.left = left;
		this.right = right;
		if (this.left != null) {
			total_weight += this.left.getTotalWeight();
		}
		if (this.right != null) {
			total_weight += this.right.getTotalWeight();
		}
	}

	@Override
	public double getTotalWeight() {
		return total_weight;
	}

	@Override
	public WeightedElement<S> select(double w) {
		if (total_weight == 0.0) {
			return null;
		}
		if ((left != null) && (w < left.getTotalWeight())) {
			return left.select(w);
		}
		if (right != null) {
			return right.select(w - left.getTotalWeight());
		}
		return null;
	}

	@Override
	public WeightedStructure<S> add(double w, S s) {
		return add(new WeightedElement<S>(w, s));
	}

	@Override
	public WeightedStructure<S> add(WeightedStructure<S> s) {
		if (s == null) {
			return this;
		}
		double increment = s.getTotalWeight();
		if (left == null) {
			// right is null
			// this.left = s;
			// this.total_weight += increment;
			// return this;
			return s;
		}
		if (right == null) {
			this.right = s;
			this.total_weight += increment;
			return this;
		}
		if ((increment >= left.getTotalWeight())
				&& (increment >= right.getTotalWeight())) {
			return new ComposedWeightedStructure<S>(this, s);
		}
		if (this.left.getTotalWeight() < this.right.getTotalWeight()) {
			this.left = this.left.add(s);
		} else {
			this.right = this.right.add(s);
		}
		this.total_weight += increment;
		return this;
	}

	@Override
	public List<WeightedElement<S>> getAll() {
		LinkedList<WeightedElement<S>> toReturn = new LinkedList<>();
		if (left != null) {
			toReturn.addAll(left.getAll());
		}
		if (right != null) {
			toReturn.addAll(right.getAll());
		}
		return toReturn;
	}

}
