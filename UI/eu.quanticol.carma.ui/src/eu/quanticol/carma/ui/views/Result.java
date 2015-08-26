package eu.quanticol.carma.ui.views;

public class Result {
	

	private double t = 0.0;
	private double m = 0.0;
	private double sd = 0.0;
	private String name = "";
	
	public Result(String name, double t, double m, double sd){
		this.t = t;
		this.m = m;
		this.sd = sd;
		this.name = name;
	}
	
	public double getT() {
		return t;
	}

	public void setT(double t) {
		this.t = t;
	}

	public double getM() {
		return m;
	}

	public void setM(double m) {
		this.m = m;
	}

	public double getSd() {
		return sd;
	}

	public void setSd(double sd) {
		this.sd = sd;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}
