/**
 * 
 */
package eu.quanticol.carma.core.ui.data;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;

/**
 * @author loreti
 *
 */
public class MeasureData {

	private String measureName;
	
	private Map<String, Object> parameters;

	public MeasureData(String measureName, Map<String, Object> map) {
		super();
		this.measureName = measureName;
		this.parameters = map;
	}

	public String getMeasureName() {
		return measureName;
	}

	public Map<String, Object> getParameters() {
		return parameters;
	}

	public static MeasureData parseMeasure( BufferedReader reader ) throws IOException {
		String measureName = reader.readLine();
		if ((measureName  == null)||(measureName .isEmpty())) {
			return null;
		}
		HashMap<String, Object> parameters = new HashMap<>();
		String name = reader.readLine();
		while (( name != null )&&(!name.isEmpty())) {
			String value = reader.readLine();
			if ((value == null)||(value.length()<2)) {
				return null;
			}
			switch (value.charAt(0)) {
			case 'D':
				parameters.put(name , Double.parseDouble(value.substring(1)) );
				break ;
			case 'I':
				parameters.put(name , Integer.parseInt(value.substring(1)) );
				break ;
			case 'B':
				parameters.put(name , Boolean.parseBoolean(value.substring(1)) );	
				break ;
			}
			name = reader.readLine();
		}
		return new MeasureData(measureName, parameters);
	}

	public static void writeTo(PrintWriter writer, MeasureData m) {
		writer.println(m.getMeasureName());
		m.parameters.forEach( new BiConsumer<String, Object>() {

			@Override
			public void accept(String name, Object value) {
				writer.println(name);
				writer.println(toValueCode(value));
			}
			
		} );
		writer.println();
	}

	protected static String toValueCode(Object value) {
		if (value instanceof Integer) {
			return "I"+value.toString();
		}
		if (value instanceof Double) {
			return "D"+value.toString();
		}
		if (value instanceof Boolean) {
			return "B"+value.toString();
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return measureName+(parameters.size()>0?parameters.toString():"");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((measureName == null) ? 0 : measureName.hashCode());
		result = prime * result + parameters.hashCode();
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		MeasureData other = (MeasureData) obj;
		if (measureName == null) {
			if (other.measureName != null)
				return false;
		} else if (!measureName.equals(other.measureName))
			return false;
		if (!parameters.equals(other.parameters))
			return false;
		return true;
	}
	
	
	
}
