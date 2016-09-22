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
package org.cmg.ml.sam.sim.tests.sier;

public class SeirStateData {
	private int inS;
	private int inI;
	private int inE;
	private int inR;
	private double totalSize;

	public SeirStateData() {
	}

	public int getInS() {
		return inS;
	}

	public void setInS(int inS) {
		this.inS = inS;
	}

	public int getInI() {
		return inI;
	}

	public void setInI(int inI) {
		this.inI = inI;
	}

	public int getInE() {
		return inE;
	}

	public void setInE(int inE) {
		this.inE = inE;
	}

	public int getInR() {
		return inR;
	}

	public void setInR(int inR) {
		this.inR = inR;
	}

	public double getTotalSize() {
		return totalSize;
	}

	public void setTotalSize(double totalSize) {
		this.totalSize = totalSize;
	}
}