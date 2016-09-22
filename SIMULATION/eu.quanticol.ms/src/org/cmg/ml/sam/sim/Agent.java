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
package org.cmg.ml.sam.sim;

import org.cmg.ml.sam.sim.util.WeightedStructure;

/**
 * 
 * 
 * @author loreti
 *
 * @param <S>
 */
public abstract class Agent<S> {

	private String name;

	public Agent() {
		this("Agent");
	}

	public Agent(String name) {
		this.name = name;
	}

	public abstract WeightedStructure<Activity> getActivities(S data);

	@Override
	public String toString() {
		return "<" + name + ">";
	}

}
