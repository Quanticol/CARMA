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
public class WeightedElement<S> implements WeightedStructure<S> {

	private double w;
	private S s;

	public WeightedElement(double w, S s) {
		this.w = w;
		this.s = s;
	}

	public double getWeight() {
		return w;
	}

	public S getElement() {
		return s;
	}

	public WeightedElement<S> residual(double w) {
		return new WeightedElement<S>(this.w - w, s);
	}

	@Override
	public double getTotalWeight() {
		return w;
	}

	@Override
	public WeightedElement<S> select(double w) {
		if (w <= this.w) {
			return this;
		}
		return null;
	}

	@Override
	public WeightedStructure<S> add(double w, S s) {
		WeightedLinkedList<S> list = new WeightedLinkedList<S>();
		list.add(this);
		list.add(w, s);
		return null;
	}

	@Override
	public WeightedStructure<S> add(WeightedStructure<S> s) {
		return new ComposedWeightedStructure<S>(this, s);
	}

	@Override
	public String toString() {
		return s+":"+w;
	}

	@Override
	public List<WeightedElement<S>> getAll() {
		LinkedList<WeightedElement<S>> list = new LinkedList<>();
		list.add(this);
		return list;
	}


}
