package eu.quanticol.carma.core.generator.actions

import eu.quanticol.carma.core.carma.OutputActionArguments
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.OutputActionArgument
import java.util.ArrayList
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import java.util.HashMap
import eu.quanticol.carma.core.carma.VariableReference
import com.google.inject.Inject
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.generator.GeneratorUtils
import eu.quanticol.carma.core.generator.ExpressionHandler
import eu.quanticol.carma.core.carma.CarmaInteger

class Values {
	
	@Inject extension LabelUtil
	@Inject extension GeneratorUtils
	@Inject extension ExpressionHandler	
	
	def String getValues(OutputActionArguments valueBlock, CarmaVariableManager manager){
		switch(valueBlock){
			NullOutputActionArguments:'''
			@Override
			protected Object getValue(CarmaStore store) {
				return new Object();
			}
			'''
			default:'''
			@Override
			protected Object getValue(CarmaStore store) {
				«defineValueBlock(valueBlock, manager)»
			}
			'''
		}
	}
	
	def String defineValueBlock(OutputActionArguments valueBlock, CarmaVariableManager manager){

		var ArrayList<OutputActionArgument> args = new ArrayList<OutputActionArgument>(valueBlock.eAllOfType(OutputActionArgument))
		var vrs = getAll(args,manager)
		'''
		int[] output = new int[«args.size»];
		«FOR item : manager.declareAllValue(vrs)»
		«item»
		«ENDFOR»
		«manager.setupStores(vrs,"")»
		«FOR item : declare(vrs,manager)»
		«item»
		«ENDFOR»
		return output;
		'''
	}
	
	def HashMap<String,VariableReference> getAll(ArrayList<OutputActionArgument> args, CarmaVariableManager manager){
		var HashMap<String,VariableReference> vrs 	= new HashMap<String,VariableReference>()
		for(arg : args){
			for(vr : arg.eAllOfType(VariableReference)){
				vrs.put(manager.cleanName(vr.asJava),vr);
			}
			for(ci : arg.eAllOfType(CarmaInteger)){
				var vr = new VariableReferenceHandler()
				vrs.put(ci.label,vr)
			}
		}
		return vrs
	}
	
	def String setupStores(CarmaVariableManager manager, HashMap<String,VariableReference> vrs, String modifier){
		'''
		«FOR key : vrs.keySet»
		«var vr = vrs.get(key)»
		«switch(vr){
			VariableReferenceHandler: ""
			default: manager.getCarmaName(key,vr).getStore("Integer.class",manager.getJavaAssign(key,vr,modifier),modifier+"store","hasAttributes")
		}»
		«ENDFOR»
		'''
	}
	
	def ArrayList<String> declare(HashMap<String,VariableReference> vrs, CarmaVariableManager manager){
		var ArrayList<String> output = new ArrayList<String>()
		var count  = 0
		for(key : vrs.keySet){
			switch(vrs.get(key)){
				VariableReferenceHandler: output.add('''output[«count»] = «key»;''')
				default: output.add('''output[«count»] = «vrs.get(key).asJava»;''')
			}
			count++
		}
		return output
	}
}