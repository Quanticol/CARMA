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
package org.cmg.ml.sam.sim.ds;

import org.apache.commons.math3.random.RandomGenerator;
import org.cmg.ml.sam.sim.Activity;
import org.cmg.ml.sam.sim.ds.TupleSpace.Node;

/**
 * @author loreti
 *
 */
public class GetActivity implements Activity {

	private Node node;

	public GetActivity(Node node) {
		this.node = node;
	}

	public Tuple getTuple() {
		return node.t;
	}

	@Override
	public boolean execute(RandomGenerator r) {
		if (node.occurrences <= 0) {
			return false;
		}
		node.occurrences--;
		return true;
	}

}
