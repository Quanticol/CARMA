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

/**
 * @author loreti
 *
 */
public class SeirState {
	
	public static final double  BETA  = 1.0;	
	public static final double  ALPHA = 2.0;
	public static final double 	GAMMA = 0.1;
	public static final double 	MU = 0.01;
	

	private SeirStateData data = new SeirStateData();

	public int getInS() {
		return data.getInS();
	}

	public int getInI() {
		return data.getInI();
	}

	public int getInE() {
		return data.getInE();
	}

	public int getInR() {
		return data.getInR();
	}

	public double getTotalSize() {
		return data.getTotalSize();
	}

	public SeirState( int s , int i , int e , int r ) {
		this.data.setInS(s);
		this.data.setInI(i);
		this.data.setInE(e);
		this.data.setInR(r);
		this.data.setTotalSize(s+i+r+e);
	}
	
	public void fromStoE() {
		this.data.setInE(this.data.getInE() + 1);
		this.data.setInS(this.data.getInS() - 1);
	}
	
	public void fromEtoI() {
		this.data.setInE(this.data.getInE() - 1);
		this.data.setInI(this.data.getInI() + 1);
	}
	
	public void fromItoR() {
		this.data.setInI(this.data.getInI() - 1);
		this.data.setInR(this.data.getInR() + 1);
	}
	
	public void fromRtoI() {
		this.data.setInR(this.data.getInR() - 1);
		this.data.setInI(this.data.getInI() + 1);
	}
	
	public double getInfectionProbability() {
		return data.getInI()/data.getTotalSize();
	}

	public double fromStoERate() {
		return BETA*getInfectionProbability();
	}
	
	public double fromEtoIRate() {
		return ALPHA;
	}
	
	public double fromItoRRate() {
		return GAMMA;
	}
	
	public double fromRtoIRate() {
		return MU;
	}

	@Override
	public int hashCode() {
		// TODO Auto-generated method stub
		return super.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		// TODO Auto-generated method stub
		return super.equals(obj);
	}

	@Override
	public String toString() {
		return "<"+data.getInS()+","+data.getInE()+","+data.getInI()+","+data.getInR()+">";
	}

	
}