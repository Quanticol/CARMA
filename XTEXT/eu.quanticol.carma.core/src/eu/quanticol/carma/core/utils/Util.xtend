package eu.quanticol.carma.core.utils

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.Process
import java.util.HashSet
import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.ArrayList
import eu.quanticol.carma.core.carma.CBND
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.Process
import eu.quanticol.carma.core.carma.ProcessComposition
import eu.quanticol.carma.core.carma.ProcessParameter
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.carma.ProcessReference
import eu.quanticol.carma.core.carma.ComponentStyle
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.InputAction
import eu.quanticol.carma.core.carma.InputActionParameters
import eu.quanticol.carma.core.carma.OutputActionArguments
import eu.quanticol.carma.core.carma.InputActionParameter
import java.math.BigInteger

class Util {
	
	def String convertName(Name name){
		var StringBuilder sb = new StringBuilder()
		var chars = name.name.toCharArray()
    	for (c : chars){
    		sb.append(c as int)
    	}
    		

    	var BigInteger mInt = new BigInteger(sb.toString());
    	return "" + mInt.intValue
	}
	
	def boolean sameName(Name name1, Name name2){
		name1.name.equals(name2.name)
	}
	
	def Action getOpposite(Action action){
		var actionName = action.name
		var isOutput = action.eAllOfType(OutputAction).size > 0
		var actions = action.getContainerOfType(Model).eAllOfType(Action)
		
		for(act : actions){
			if(act.name.sameName(actionName))
				if(isOutput){
					if(act.eAllOfType(OutputAction).size == 0){
						if(act.eAllOfType(InputActionParameters).get(0).parameters.size == 
							action.eAllOfType(OutputActionArguments).get(0).outputArguments.size)
							return act
					}
				} else {
					if(act.eAllOfType(OutputAction).size > 0){
						if(action.eAllOfType(InputActionParameters).get(0).parameters.size == 
							act.eAllOfType(OutputActionArguments).get(0).outputArguments.size)
							return act
					}
				}
		}
		
		return null;
	}
	
	def int getIndex(InputActionParameters inputActionParameters, InputActionParameter inputActionParameter){
		var count = 0;
		for(parameter : inputActionParameters.parameters){
			if((parameter as InputActionParameter).name.sameName(inputActionParameter.name)){
				return count;
			}
			count++ 
		}
		return -1
	}
	
	def ArrayList<Process> getInitialState(CBND cbnd){
		
		var definition = cbnd.name.getContainerOfType(Component) as ComponentBlockDefinition
		var ProcessParameter processParameter = null 
		if(definition.componentSignature.componentParameters.eAllOfType(ProcessParameter).size > 0)
			processParameter = definition.componentSignature.componentParameters.eAllOfType(ProcessParameter).get(0)
			
		var String parameterLabel = ""
		if(processParameter != null)
			parameterLabel = processParameter.name.name
			
		var ProcessComposition initialisation = definition.componentBlock.initBlock.init
		var ArrayList<String> array = new ArrayList<String>()
		initialisation.stringArray(array)
		
		var ArrayList<Process> toReturn = new ArrayList<Process>()
		
		initialisation.processArray(toReturn)
		
		if(array.contains(parameterLabel)){
			var i = array.indexOf(parameterLabel)
			toReturn.remove(i)
			var ProcessComposition argumentProcessComposition = null 
			if(cbnd.arguments.eAllOfType(ProcessComposition).size > 0){
				argumentProcessComposition = cbnd.arguments.eAllOfType(ProcessComposition).get(0)
				argumentProcessComposition.processArray(toReturn)
			}
		}
		return toReturn
	}
	
	def void stringArray(ProcessComposition processComposition, ArrayList<String> array){
		switch(processComposition){
			ParallelComposition	: {processComposition.left.stringArray(array) processComposition.right.stringArray(array)}
			ProcessReference	: {array.add(processComposition.expression.name)}
		}
	}
	
	def void processArray(ProcessComposition processComposition, ArrayList<Process> array){
		switch(processComposition){
			ParallelComposition	: {processComposition.left.processArray(array) processComposition.right.processArray(array)}
			ProcessReference	: {array.add(processComposition.expression.getContainerOfType(Process))}
		}
	}
	
	def Tree getTree(CBND cbnd){
		
		var HashSet<Process> processes = new HashSet<Process>()
		var HashSet<Process> processes2 = new HashSet<Process>()
		var Tree tree = null;
		
		processes.addAll(cbnd.initialState)
		
		for(p : processes)
				processes2.addAll(p.allReferences)
		
		processes.addAll(processes2);
		
		if(processes.size > 0){
			tree = new Tree(processes);
		}
		
		return tree
	}	
	
	/**
	 * Return a List of all Processes associated with the given one. 
	 * All Processes that transition from, or to, the Process argument. 
	 * <p>
	 * @author 	CDW <br>
	 * @param	Process <br>
	 * @return	ArrayList<Process>
	 */
	def ArrayList<Process> maximumFixedPoint(Process p1, boolean includeSelf){
		
		
		var HashSet<Process> buffer1 = new HashSet<Process>()
		var HashSet<Process> buffer2 = new HashSet<Process>()
		
		if(includeSelf)
			buffer1.add(p1)
		else
			buffer1.addAll(getReferences(p1))
		
		while(buffer1.size > buffer2.size){
			
			buffer2.addAll(buffer1)
			
			for(Process p2 : buffer2){
				if(includeSelf)
					buffer1.addAll(getAllReferences(p2))
				else
					buffer1.addAll(getReferences(p2))
			}
		}
		
		var ArrayList<Process> output = new ArrayList<Process>(buffer1)
		
		return output
		
	}
	
	/**
	 * Return all Processes referenced by the argument Process. Both parents and children.
	 * 
	 * @author 	CDW
	 * @param	Process
	 * @return	HashSet<Process>
	 */
	def HashSet<Process> getAllReferences(Process p1){
		
		var HashSet<Process> output = new HashSet<Process>()
		
		for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
			for(ProcessExpressionReference pr : p2.eAllOfType(ProcessExpressionReference))
				if(p1.name.name.equals(pr.expression.name))
					output.add(p2)
		
		for(ProcessExpressionReference pr : p1.eAllOfType(ProcessExpressionReference))
			for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
				if(p2.name.name.equals(pr.expression.name))
					output.add(p2)
					
		return output 
		
	}
	
	/**
	 * Return all Processes referenced by the argument Process. Only children.
	 * 
	 * @author 	CDW
	 * @param	Process
	 * @return	HashSet<Process>
	 */
	def HashSet<Process> getReferences(Process p1){
		
		var HashSet<Process> output = new HashSet<Process>()
		
		for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
			for(ProcessExpressionReference pr : p2.eAllOfType(ProcessExpressionReference))
				if(p1.name.name.equals(pr.expression.name))
					output.add(p2)
							
		return output 
		
	}
}