package eu.quanticol.carma.core.generator.ms.main

import eu.quanticol.carma.core.carma.CarmaBoolean
import eu.quanticol.carma.core.carma.CarmaDouble
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.MeasureVariableDeclaration
import eu.quanticol.carma.core.carma.MeasureVariableDeclarations
import eu.quanticol.carma.core.carma.PrimitiveTypes
import eu.quanticol.carma.core.carma.Range
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class MainJavaniser {
	
//	def ArrayList<String> array(MeasureVariableDeclaration measureVariableDeclaration){
//		var ArrayList<String> toReturn = new ArrayList<String>();
//		measureVariableDeclaration.assign.array(toReturn)
//		return toReturn
//	}
//	
//	def void array(PrimitiveTypes pts, ArrayList<String> array) {
//		switch (pts) {
//			CarmaDouble: {array.add("//eu.quanticol.carma.core.generator.ms.function.javanise.CarmaDouble")}
//			CarmaInteger: {array.add(pts.javanise)}
//			CarmaBoolean: {array.add("//eu.quanticol.carma.core.generator.ms.function.javanise.CarmaBoolean")}
//			Range: pts.array(array)
//		}
//	}
//		
//	def void array(Range pt, ArrayList<String> array) {
//		for(var i = pt.min; i <= pt.max; i++){
//			array.add(""+i)
//		}
//	}
//		
//	def String javanise(CarmaInteger pt) {
//		if (pt.negative != null)
//			return "-" + pt.value
//		else
//			return "" + pt.value
//	}
//	
//	def void cartesianProduct(ArrayList<ArrayList<String>> in, ArrayList<ArrayList<String>> out){
//		if(in.size() > 1){
//			var ArrayList<String> head = in.remove(0);
//			var ArrayList<ArrayList<String>> exit = new ArrayList<ArrayList<String>>();
//			cartesianProduct(in,out);
//			for(var int i = 0; i < out.size(); i++){
//				for(String item : head){
//					var ArrayList<String> inter = new ArrayList<String>();
//					inter.add(item);
//					inter.addAll(out.get(i));
//					exit.add(inter);
//				}
//			}
//			out.clear();
//			out.addAll(exit);
//		} else {
//			var ArrayList<String> head = in.remove(0);
//			for(String item : head){
//				var ArrayList<String> tail = new ArrayList<String>();
//				tail.add(item);
//				out.add(tail);
//			}
//		}
//	}	
}