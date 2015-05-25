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

import java.util.HashMap;
import java.util.LinkedList;

import org.cmg.ml.sam.sim.util.ComposedWeightedStructure;
import org.cmg.ml.sam.sim.util.WeightedStructure;
import org.cmg.ml.sam.sim.util.Weighter;

/**
 * @author loreti
 *
 */
public class TupleSpace {

	private Node root;
	private Weighter<Tuple> weighter;

	public TupleSpace() {
		this.root = new Node();
	}

	public class Node {

		int occurrences;

		Tuple t;

		HashMap<Object, Node> nodes;

		public Node() {
			this.occurrences = 0;
			this.t = new Tuple();
			this.nodes = new HashMap<Object, Node>();
		}

		public Node get(Object v) {
			Node n = nodes.get(v);
			if (n == null) {
				n = new Node();
				nodes.put(v, n);
			}
			return n;
		}

		public LinkedList<Node> get(TemplateField f) {
			LinkedList<Node> toReturn = new LinkedList<Node>();
			for (Object o : nodes.keySet()) {
				if (f.match(o)) {
					toReturn.add(nodes.get(o));
				}
			}
			return toReturn;
		}

	}

	public boolean put(Tuple t) {
		Node node = getNode(t);
		if (node.t == null) {
			node.t = t;
		}
		node.occurrences++;
		return true;
	}

	public WeightedStructure<GetActivity> get(Template t) {
		LinkedList<Node> lst = collect(t);
		ComposedWeightedStructure<GetActivity> ws = new ComposedWeightedStructure<GetActivity>();
		for (Node node : lst) {
			if (node.occurrences > 0) {
				ws.add(weight(node), new GetActivity(node));
			}
		}
		return ws;
	}

	public WeightedStructure<Tuple> query(Template t) {
		LinkedList<Node> lst = collect(t);
		ComposedWeightedStructure<Tuple> ws = new ComposedWeightedStructure<Tuple>();
		for (Node node : lst) {
			if (node.occurrences > 0) {
				ws.add(weight(node), node.t);
			}
		}
		return ws;
	}

	private Node getNode(Tuple t) {
		Node toReturn = root;
		for (int i = 0; i < t.size(); i++) {
			toReturn = toReturn.get(t.get(i));
		}
		return toReturn;
	}

	private LinkedList<Node> collect(Template t) {
		LinkedList<Node> pending = new LinkedList<Node>();
		pending.add(root);
		for (int i = 0; i < t.size(); i++) {
			LinkedList<Node> nextPending = new LinkedList<Node>();
			for (Node node : pending) {
				nextPending.addAll(node.get(t.get(i)));
			}
			pending = nextPending;
		}
		return pending;
	}

	public int copiesOf(Tuple t) {
		return getNode(t).occurrences;
	}

	public double weightOf(Tuple t) {
		return weight(getNode(t));
	}

	public double weightOf(Template t) {
		LinkedList<Node> lst = collect(t);
		double d = 0.0;
		for (Node node : lst) {
			if (node.occurrences > 0) {
				d += weight(node);
			}
		}
		return d;
	}

	private double weight(Node node) {
		if (weighter == null) {
			return node.occurrences;
		} else {
			return weighter.weight(node.t, node.occurrences);
		}
	}

	public int copiesOf(Template t) {
		LinkedList<Node> lst = collect(t);
		int count = 0;
		for (Node node : lst) {
			count += node.occurrences;
		}
		return count;
	}

}
