package eu.quanticol.carma.core.utils

import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.ActionName
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclaration
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.ComponentLineDefinition
import eu.quanticol.carma.core.carma.ComponentLineDefinitionSpawn
import eu.quanticol.carma.core.carma.ComponentLineForStatement
import eu.quanticol.carma.core.carma.ComponentLineStyle
import eu.quanticol.carma.core.carma.ComponentName
import eu.quanticol.carma.core.carma.ComponentStyle
import eu.quanticol.carma.core.carma.Environment
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAState
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.InitBlock
import eu.quanticol.carma.core.carma.InputAction
import eu.quanticol.carma.core.carma.MacroExpressionReference
import eu.quanticol.carma.core.carma.MacroName
import eu.quanticol.carma.core.carma.MethodDefinition
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.Process
import eu.quanticol.carma.core.carma.ProcessExpression
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import eu.quanticol.carma.core.carma.ProcessName
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.VariableName
import eu.quanticol.carma.core.carma.VariableType
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet
import java.util.List

import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.StoreLine
import eu.quanticol.carma.core.carma.StoreBlock
import eu.quanticol.carma.core.carma.InputActionArguments
import com.google.inject.Inject
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.carma.Name
import eu.quanticol.carma.core.carma.Processes
import eu.quanticol.carma.core.carma.RecordDeclarations
import eu.quanticol.carma.core.carma.Records
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.ProcessesBlock
import eu.quanticol.carma.core.carma.Methods
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionParallel
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionAll
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressionComponentAllStates
import eu.quanticol.carma.core.carma.EnvironmentMeasure
import eu.quanticol.carma.core.carma.ProcessExpressionChoice
import eu.quanticol.carma.core.carma.ProcessExpressionLeaf
import eu.quanticol.carma.core.carma.ProcessExpressionGuard
import eu.quanticol.carma.core.carma.ProcessExpressionAction
import eu.quanticol.carma.core.carma.CBND
import eu.quanticol.carma.core.carma.ComponentBlockStyle
import eu.quanticol.carma.core.carma.MultiCast
import eu.quanticol.carma.core.carma.SpontaneousAction
import eu.quanticol.carma.core.carma.OutputActionArgument
import eu.quanticol.carma.core.carma.Guard
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.carma.LineSystem
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.ComponentArgument
import eu.quanticol.carma.core.carma.ComponentBlockDefinitionArguments
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferenceThis
import eu.quanticol.carma.core.carma.VariableReferenceReciever
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.RecordReferenceReciever
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableDeclarationEnum

class Util {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	
	/**
	 * Check if the names are the same!
	 */
	def boolean sameName(Name name1, Name name2){
		name1.label.equals(name2.label)
	}
	
	/**
	 * Make sure all variable references are actually declared in this model. This prevents cross model referencing, if there is another .carma model
	 * declared inside the project.
	 */
	def boolean isDeclaredInThisModel(VariableReference vr){
		
		var boolean output = false
		
		//methods @ anywhere in the method 
		if(vr.getContainerOfType(MethodDefinition) != null){
			var declarations 	= vr.getContainerOfType(MethodDefinition).eAllOfType(VariableDeclaration)
			var arguments 		= vr.getContainerOfType(MethodDefinition).eAllOfType(VariableType)
			
			for(dec : declarations)
				output = output || dec.name.sameName(vr.name)
				
			for(dec : arguments)
				output = output || dec.name.sameName(vr.name)
				
			
		}
		
		//componentblock @ StoreBlock || componentdefinition arguments
		if(vr.getContainerOfType(ComponentBlockDefinition) != null){
			var declarations 	= vr.getContainerOfType(ComponentBlockDefinition).eAllOfType(VariableDeclaration)
			var arguments 		= vr.getContainerOfType(ComponentBlockDefinition).eAllOfType(VariableType)
			
			for(dec : declarations)
				output = output || dec.name.sameName(vr.name)
				
			for(dec : arguments)
				output = output || dec.name.sameName(vr.name)
				
			if(vr.getContainerOfType(Process) != null)	
				if(vr.getContainerOfType(Process).eAllOfType(InputAction).size > 0){
					if(vr.getContainerOfType(InputAction).eAllOfType(InputActionArguments).size > 0){
						for(dec : vr.getContainerOfType(InputAction).eAllOfType(InputActionArguments).get(0).inputArguments)
							output = output || dec.sameName(vr.name)
					}
				}
			
		}
		
		//processes && !componentblock @ StoreLine or StoreBlock
		if((vr.getContainerOfType(ComponentBlockDefinition) == null) && (vr.getContainerOfType(Processes) != null)){
			var componentAndDeclarations 	= vr.getContainerOfType(Process).getComponentAndDeclarations
			for(key : componentAndDeclarations.keySet)
				for(dec : componentAndDeclarations.get(key))
					output = output || dec.name.sameName(vr.name)
					
			if(vr.getContainerOfType(Process) != null)	
				if(vr.getContainerOfType(Process).eAllOfType(InputAction).size > 0){
					if(vr.getContainerOfType(InputAction).eAllOfType(InputActionArguments).size > 0){
						for(dec : vr.getContainerOfType(InputAction).eAllOfType(InputActionArguments).get(0).inputArguments)
							output = output || dec.sameName(vr.name)
					}
				}
		}
		
		//environment
		if(vr.getContainerOfType(Environment) != null){
			var rate   = vr.getContainerOfType(Rate)
			var update = vr.getContainerOfType(EnvironmentUpdate)
			var prob   = vr.getContainerOfType(Probability)
			
			var ActionStub actionStub = null
			
			if(rate != null)
				actionStub = rate.eAllOfType(ActionStub).get(0)
			if(update != null)
				actionStub = update.eAllOfType(ActionStub).get(0)
			if(prob != null)
				actionStub = prob.eAllOfType(ActionStub).get(0)
				
			for(p : actionStub.processes){
				var componentAndDeclarations = p.getComponentAndDeclarations
				for(key : componentAndDeclarations.keySet)
					for(dec : componentAndDeclarations.get(key))
						output = output || dec.name.sameName(vr.name)
			}
			
			for(dec : vr.getContainerOfType(Model).environmentAttributes)
				output = output || dec.name.sameName(vr.name)
					
		}
		
		if(vr.getContainerOfType(ComponentBlockForStatement) != null || vr.getContainerOfType(ComponentLineForStatement) != null ){
			if(vr.getContainerOfType(ComponentBlockForStatement) != null){
				for(dec : vr.getContainerOfType(ComponentBlockForStatement).eAllOfType(VariableDeclaration))
					output = output || dec.name.sameName(vr.name)
			}else{
				for(dec : vr.getContainerOfType(ComponentLineForStatement).eAllOfType(VariableDeclaration))
					output = output || dec.name.sameName(vr.name)
			}
		}
		
		//measure
		if(vr.getContainerOfType(Measure) != null){
			var ArrayList<String> names = new ArrayList<String>()
			(vr.getContainerOfType(Measure).measure as EnvironmentMeasure).componentReference.getComponentName(names)
			var boolean seen = false
			output = true
			for(name : names){
				var component = name.getComponent(vr.getContainerOfType(Model))
				for(dec : component.eAllOfType(VariableDeclaration)){
					seen = seen || dec.name.sameName(vr.name)
				}
				output = output && seen
			}
			
		}
		
		return output
		
	}
	
	def ArrayList<Name> getNames(RecordDeclarations rds){
		var ArrayList<Name> names = new ArrayList<Name>()
			if((rds as Records).recordDeclarations != null){
				for(rd : (rds as Records).recordDeclarations)
					names.add((rd as RecordDeclaration).name)
			}
		return names
	}
	
	def ArrayList<Name> getNames(ProcessesBlock psb){
		var ArrayList<Name> names = new ArrayList<Name>()
			if(psb.processes != null){
				for(p : psb.processes)
					names.add(p.name)
			}
		return names
	}
	
	def ArrayList<Name> getNames(Processes ps){
		var ArrayList<Name> names = new ArrayList<Name>()
			if(ps.processes != null){
				for(p : ps.processes)
					names.add(p.name)
			}
		return names
	}
	
	def ArrayList<Name> getNames(StoreBlock sb){
		var ArrayList<Name> names = new ArrayList<Name>()
			if(sb.attributes != null){
				for(p : sb.attributes)
					names.add(p.name)
			}
		return names
	}
	
	def ArrayList<Name> getNames(StoreLine sl){
		var ArrayList<Name> names = new ArrayList<Name>()
			if(sl.attributes != null){
				for(p : sl.attributes)
					names.add(p.name)
			}
		return names
	}
	
	def ArrayList<Name> getNames(Methods ms){
		var ArrayList<Name> names = new ArrayList<Name>()
			if(ms.methods != null){
				for(p : ms.methods)
					names.add(p.name)
			}
		return names
	}
	
	
//	/**
//	 * Given a RecordReference, state if it is in this model. This is to protect against using references in other .carma models.
//	 */
//	def boolean isDeclaredInThisModel(RecordReference rr){
//		var boolean output = false
//		
//		//methods @ anywhere in the method 
//		if(rr.getContainerOfType(MethodDefinition) != null){
//			var declarations 	= rr.getContainerOfType(MethodDefinition).eAllOfType(VariableDeclaration)
//			var arguments 		= rr.getContainerOfType(MethodDefinition).eAllOfType(VariableType)
//			
//			for(dec : declarations)
//				if(dec.name.sameName(rr.name))
//					if((dec as VariableDeclarationRecord).assign != null)
//						for(name : (dec as VariableDeclarationRecord).assign.getNames)
//							output = output || name.sameName(rr.record)
//						
//				
//			for(dec : arguments)
//				output = output || dec.name.sameName(rr.name)
//				
//			
//		}
//		
//		//componentblock @ StoreBlock || componentdefinition arguments
//		if(rr.getContainerOfType(ComponentBlockDefinition) != null){
//			var declarations 	= rr.getContainerOfType(ComponentBlockDefinition).eAllOfType(VariableDeclaration)
//			var arguments 		= rr.getContainerOfType(ComponentBlockDefinition).eAllOfType(VariableType)
//			
//			for(dec : declarations)
//				if(dec.name.sameName(rr.name))
//					if((dec as VariableDeclarationRecord).assign != null)
//						for(name : (dec as VariableDeclarationRecord).assign.getNames)
//							output = output || name.sameName(rr.record)
//				
//			for(dec : arguments)
//				output = output || dec.name.sameName(rr.name)
//				
//			
//		}
//		
//		//processes && !componentblock @ StoreLine or StoreBlock
//		if((rr.getContainerOfType(ComponentBlockDefinition) == null) && (rr.getContainerOfType(Processes) != null)){
//			var componentAndDeclarations 	= rr.getContainerOfType(Process).getComponentAndDeclarations
//			for(key : componentAndDeclarations.keySet)
//				for(dec : componentAndDeclarations.get(key))
//					if(dec.name.sameName(rr.name))
//						if((dec as VariableDeclarationRecord).assign != null)
//							for(name : (dec as VariableDeclarationRecord).assign.getNames)
//								output = output || name.sameName(rr.record)
//					
//			
//		}
//		
//		//environment
//		if(rr.getContainerOfType(Environment) != null){
//			var rate   = rr.getContainerOfType(Rate)
//			var update = rr.getContainerOfType(EnvironmentUpdate)
//			var prob   = rr.getContainerOfType(Probability)
//			
//			var ActionStub actionStub = null
//			
//			if(rate != null)
//				actionStub = rate.eAllOfType(ActionStub).get(0)
//			if(update != null)
//				actionStub = update.eAllOfType(ActionStub).get(0)
//			if(prob != null)
//				actionStub = prob.eAllOfType(ActionStub).get(0)
//				
//			for(p : actionStub.processes){
//				var componentAndDeclarations = p.getComponentAndDeclarations
//				for(key : componentAndDeclarations.keySet)
//					for(dec : componentAndDeclarations.get(key))
//						if(dec.name.sameName(rr.name))
//							if((dec as VariableDeclarationRecord).assign != null)
//								for(name : (dec as VariableDeclarationRecord).assign.getNames)
//									output = output || name.sameName(rr.record)
//			}
//			
//			for(dec : rr.getContainerOfType(Model).environmentAttributes)
//				if(dec.name.sameName(rr.name))
//					if((dec as VariableDeclarationRecord).assign != null)
//						for(name : (dec as VariableDeclarationRecord).assign.getNames)
//							output = output || name.sameName(rr.record)
//					
//		}
//		
//		//measure
//		if(rr.getContainerOfType(Measure) != null){
//			var ArrayList<String> names = new ArrayList<String>()
//			(rr.getContainerOfType(Measure).measure as EnvironmentMeasure).componentReference.getComponentName(names)
//			var boolean seen = false
//			output = true
//			for(name : names){
//				var component = name.getComponent(rr.getContainerOfType(Model))
//				for(dec : component.eAllOfType(VariableDeclaration))
//					if(dec.name.sameName(rr.name))
//						if((dec as VariableDeclarationRecord).assign != null)
//							for(n : (dec as VariableDeclarationRecord).assign.getNames)
//								seen = seen || n.sameName(rr.record)
//				output = output && seen
//			}
//			
//		}
//		
//		return output
//	}
	
	/**
	 * Given a componentName and model, return the component
	 */
	def Component getComponent(String name, Model model){
		var Component c = null
		for(component : model.eAllOfType(ComponentBlockDefinition))
			if((component.name as ComponentName).name.equals(name))
				c = component
		for(component : model.eAllOfType(ComponentLineDefinition))
			if((component.name as ComponentName).name.equals(name))
				c = component		
		return c
	}
	
	/**
	 * Get all componentNames referenced by MacroExpression
	 */
	def void getComponentName(EnvironmentMacroExpressions eme, ArrayList<String> output){
		switch(eme){
			EnvironmentMacroExpressionParallel:				{
				eme.left.getComponentName(output)
				eme.right.getComponentName(output)
				}
			EnvironmentMacroExpressionAll:					{
				output.addAll(eme.getContainerOfType(Model).getAllComponentNames())
			}
			EnvironmentMacroExpressionComponentAllStates: {
				output.add((eme as EnvironmentMacroExpressionComponentAllStates).comp.name)
			}
			EnvironmentMacroExpressionComponentAState:	{
				output.add((eme as EnvironmentMacroExpressionComponentAllStates).comp.name)
			}
		}
		
	}
	
	/**
	 * Given a model, get all component names
	 */
	def HashSet<String> getAllComponentNames(Model model){
		var HashSet<String> output = new HashSet<String>()
		
		for(componentName : model.eAllOfType(ComponentName))
			output.add(componentName.label)
		
		return output
	}
	
	/**
	 * Get opposite action
	 */
	def ArrayList<OutputAction> getSender(InputAction ia){
		var ArrayList<OutputAction> output = new ArrayList<OutputAction>()
		for(oa : ia.getContainerOfType(Model).eAllOfType(OutputAction))
			if((oa.getContainerOfType(Action).name as ActionName).name.equals((ia.getContainerOfType(Action).name as ActionName).name))
				output.add(oa)
		return output
	}
	
	/**
	 * Get opposite action
	 */
	def ArrayList<InputAction> getReceiver(OutputAction oa){
		var ArrayList<InputAction> output = new ArrayList<InputAction>()
		for(ia : oa.getContainerOfType(Model).eAllOfType(InputAction))
			if((ia.getContainerOfType(Action).name as ActionName).name.equals((oa.getContainerOfType(Action).name as ActionName).name))
				output.add(ia)
		return output
	}


	/**
	 * @see InputActionArguments
	 * needs to know what position it is in the argument list
	 * the output argument should not be the same name
	 */
	def HashSet<String> getTypesInputActionArguments(VariableName vn){
		var HashSet<String> output = new HashSet<String>()
		var action = vn.getContainerOfType(Action)
		var args = vn.getContainerOfType(InputActionArguments)
		var count = 0
		var index = 0
		
		if(action != null){
			
			for(arg : args.inputArguments){
				if((arg as VariableName).name.equals(vn.name))
					index = count
				else
					count++
			}
			
			if(action.eAllOfType(InputAction).size > 0){
				for(oas : action.eAllOfType(InputAction).get(0).getSender){
					if(oas.outputActionArguments.outputArguments.size > index)
						if(oas.outputActionArguments != null)
							output.add((oas.outputActionArguments.outputArguments.get(index) as OutputActionArgument).type.toString)
						else
							output.add("null")
					else
						output.add("null")
				}
			}
		}
		
		return output
	}
	
	/** 
	 * @see ComponentBlockForStatement
	 * 
	 */
	def HashSet<String> getTypesComponentBlockForStatement(VariableName vn){
		var HashSet<String> output = new HashSet<String>()
		if(vn.getContainerOfType(ComponentBlockForStatement) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
				new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentBlockForStatement).eAllOfType(VariableDeclaration))
			))
		}
		return output
	}
	
	/** 
	 * @see ComponentBlockForStatement
	 * 
	 */
	def HashSet<String> getTypesComponentLineForStatement(VariableName vn){
		var HashSet<String> output = new HashSet<String>()
		if(vn.getContainerOfType(ComponentLineForStatement) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
				new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentLineForStatement).eAllOfType(VariableDeclaration))
			))
		}
		return output
	}
	
	
	/** 
	 * @see MethodAtomic
	 * 
	 */
	def HashSet<String> getTypesMethodAtomicVariable(VariableName vn){
		var HashSet<String> output = new HashSet<String>()
		if(vn.getContainerOfType(MethodDefinition) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
				new ArrayList<VariableDeclaration>(vn.getContainerOfType(MethodDefinition).eAllOfType(VariableDeclaration))
			))
			
			output.addAll(vn.name.getVariableTypeTypes(
				new ArrayList<VariableType>(vn.getContainerOfType(MethodDefinition).eAllOfType(VariableType))
			))
		}
		return output
	}
	
	
	/**
	 * PFD(arg_0,arg_1,...arg_N)
	 * @see PredefinedMethodDeclarationArgument
	 */
	def HashSet<String> getTypesPredefinedMethodDeclarationArgument(VariableName vn){
		var HashSet<String> output = new HashSet<String>()
		
		//StoreBlock ->	componentArgs
		if(vn.getContainerOfType(StoreBlock) != null ){
			output.addAll(vn.name.getVariableTypeTypes(
				new ArrayList<VariableType>(vn.getContainerOfType(ComponentBlockDefinition).eAllOfType(VariableType))
			))
		}
		
		//StoreLine	-> componentLineForStatement
		if(vn.getContainerOfType(StoreLine) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
					new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentLineForStatement).eAllOfType(VariableDeclaration))
			))
		}
		
		//Update -> Store
		if(vn.getContainerOfType(Action) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
				new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentBlockDefinition).eAllOfType(VariableDeclaration))
			))
		}
		
		//EUpdate ->	EStore
		if(vn.getContainerOfType(Environment) != null ){
			var rate   = vn.getContainerOfType(Rate)
			var update = vn.getContainerOfType(EnvironmentUpdate)
			var prob   = vn.getContainerOfType(Probability)
			
			var ActionStub actionStub = null
			
			if(rate != null)
				actionStub = rate.eAllOfType(ActionStub).get(0)
			if(update != null)
				actionStub = update.eAllOfType(ActionStub).get(0)
			if(prob != null)
				actionStub = prob.eAllOfType(ActionStub).get(0)
				
			for(p : actionStub.processes){
				var componentVariableMap = p.getComponentAndDeclarations
				for(component : componentVariableMap.keySet)
					output.addAll(vn.name.getVariableDeclarationTypes(componentVariableMap.get(component)))
			}
			
			output.addAll(vn.name.getVariableDeclarationTypes(vn.getContainerOfType(Model).environmentAttributes))
		}
		
		//ComponentLineForStatement -> 	VariableDeclaration
		if(vn.getContainerOfType(ComponentLineForStatement) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
					new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentLineForStatement).eAllOfType(VariableDeclaration))
			))
		}
		
		//ComponentBlockForStatement ->	VariableDeclaration
		if(vn.getContainerOfType(ComponentBlockForStatement) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
					new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentBlockForStatement).eAllOfType(VariableDeclaration))
			))
		}
		
		//MethodDefinition	->	VariableDeclaration
		if(vn.getContainerOfType(MethodDefinition) != null ){
			output.addAll(vn.name.getVariableDeclarationTypes(
				new ArrayList<VariableDeclaration>(vn.getContainerOfType(MethodDefinition).eAllOfType(VariableDeclaration))
			))
			
			output.addAll(vn.name.getVariableTypeTypes(
				new ArrayList<VariableType>(vn.getContainerOfType(MethodDefinition).eAllOfType(VariableType))
			))
		}
		return output
	}
	
	/**
	 * @see VariableDeclaration
	 */
	def HashSet<String> getTypesVariableDeclaration(VariableName vn){
		var HashSet<String> output = new HashSet<String>()
		if(vn.getContainerOfType(ComponentBlockDefinition) != null){
			output.addAll(vn.name.getVariableTypeTypes(
					new ArrayList<VariableType>(vn.getContainerOfType(ComponentBlockDefinition).eAllOfType(VariableType))
			))
		} else {
			output.addAll(vn.name.getVariableTypeTypes(
					new ArrayList<VariableType>(vn.getContainerOfType(ComponentLineDefinition).eAllOfType(VariableType))
			))
		}
		return output
	}
	
	/**
	 * Search for all types associated with this reference
	 */
	def HashSet<String> getTypes(VariableReference vr){
		var HashSet<String> output = new HashSet<String>()
		
		//ProcessExpression
		if(vr.getContainerOfType(ProcessExpression) != null){

			if(vr.getContainerOfType(Process) != null){
				
				var componentVariableMap = vr.getContainerOfType(Process).getComponentAndDeclarations
				
				for(component : componentVariableMap.keySet){
					output.addAll((vr.name as VariableName).name.getVariableDeclarationTypes(componentVariableMap.get(component)))
				}
					
			}
			if(output.size == 0 && (vr.getContainerOfType(Action).eAllOfType(InputActionArguments).size > 0 )){
				var action = vr.getContainerOfType(Action)
				var args = vr.getContainerOfType(InputActionArguments)
				var count = 0
				var index = 0
				
				if(action != null){
					for(arg : args.inputArguments){
						if((arg as VariableName).name.equals((vr.name as VariableName).name))
							index = count
						else
							count++
					}
					
					if(action.eAllOfType(InputAction).size > 0){
						for(oas : action.eAllOfType(InputAction).get(0).getSender){
							if(oas.outputActionArguments.outputArguments.size > index)
								output.add((oas.outputActionArguments.outputArguments.get(index) as VariableReference).type.toString)
							else
								output.add("null")
						}
					}
					else
						output.add("null")
				}
			}
		}
		//Environment
		if(vr.getContainerOfType(Environment) != null){
			var rate   = vr.getContainerOfType(Rate)
			var update = vr.getContainerOfType(EnvironmentUpdate)
			var prob   = vr.getContainerOfType(Probability)
			
			var ActionStub actionStub = null
			
			if(rate != null)
				actionStub = rate.eAllOfType(ActionStub).get(0)
			if(update != null)
				actionStub = update.eAllOfType(ActionStub).get(0)
			if(prob != null)
				actionStub = prob.eAllOfType(ActionStub).get(0)
				
			for(p : actionStub.processes){
				var componentVariableMap = p.getComponentAndDeclarations
				for(component : componentVariableMap.keySet)
					output.addAll((vr.name as VariableName).name.getVariableDeclarationTypes(componentVariableMap.get(component)))
			}
			
			output.addAll((vr.name as VariableName).name.getVariableDeclarationTypes(vr.getContainerOfType(Model).environmentAttributes))
			
		}
		
		//Component
		if(vr.getContainerOfType(ComponentBlockForStatement) != null || vr.getContainerOfType(ComponentLineForStatement) != null ){
			if(vr.getContainerOfType(ComponentBlockForStatement) != null){
				output.addAll((vr.name as VariableName).name.getVariableDeclarationTypes(
					new ArrayList<VariableDeclaration>(vr.getContainerOfType(ComponentBlockForStatement).eAllOfType(VariableDeclaration))
				))
			}else{
				output.addAll((vr.name as VariableName).name.getVariableDeclarationTypes(
					new ArrayList<VariableDeclaration>(vr.getContainerOfType(ComponentLineForStatement).eAllOfType(VariableDeclaration))
				))
			}
		}
		//Method
		if(vr.getContainerOfType(MethodDefinition) != null ){
			output.addAll((vr.name as VariableName).name.getVariableDeclarationTypes(
				new ArrayList<VariableDeclaration>(vr.getContainerOfType(MethodDefinition).eAllOfType(VariableDeclaration))
			))
			
			output.addAll((vr.name as VariableName).name.getVariableTypeTypes(
				new ArrayList<VariableType>(vr.getContainerOfType(MethodDefinition).eAllOfType(VariableType))
			))
		}
		
		//Measure
		if(vr.getContainerOfType(Measure) != null){
			var ArrayList<String> names = new ArrayList<String>()
			(vr.getContainerOfType(Measure).measure as EnvironmentMeasure).componentReference.getComponentName(names)
			for(name : names){
				var component = name.getComponent(vr.getContainerOfType(Model))
				output.addAll((vr.name as VariableName).name.getVariableDeclarationTypes(
					new ArrayList<VariableDeclaration>(component.eAllOfType(VariableDeclaration))
				))
			}
			
		}
		//Store
		if(vr.getContainerOfType(StoreBlock) != null){
			output.addAll((vr.name as VariableName).name.getVariableTypeTypes(
				new ArrayList<VariableType>(vr.getContainerOfType(ComponentBlockDefinition).eAllOfType(ComponentBlockDefinitionArguments).get(0).eAllOfType(VariableType)
				)
			))
		}
		
		return output
	}
	
	/**
	 * Return list of types that have this variable name
	 * @author CDW
	 */
	def ArrayList<String> getVariableDeclarationTypes(String variableName, ArrayList<VariableDeclaration> variables){
		var ArrayList<String> output = new ArrayList<String>()
		for(variable : variables)
			if((variable.name as VariableName).name.equals(variableName))
				output.add(variable.type)
		
		return output
	}
	
	/**
	 * Return list of types that have this variable name
	 * @author CDW
	 */
	def ArrayList<String> getVariableTypeTypes(String variableName, ArrayList<VariableType> variables){
		var ArrayList<String> output = new ArrayList<String>()
		
		for(variable : variables)
			if((variable.name as VariableName).name.equals(variableName))
				output.add(variable.type)
		
		return output
	}
	
	def ArrayList<VariableDeclaration> getEnvironmentAttributes(Model model){
		return new ArrayList<VariableDeclaration>(model.eAllOfType(Environment).get(0).eAllOfType(VariableDeclaration))
	}
	
	/**
	 * Return the components with attributes related to this Process 
	 * 
	 * @author 	CDW
	 * @param	Process
	 * @return	HashMap<Integer,ArrayList<String>>
	 */
	def HashMap<Component,ArrayList<VariableDeclaration>> getComponentAndDeclarations(Process p1){
		
		var output = new HashMap<Component,ArrayList<VariableDeclaration>>
		
		if(p1.getContainerOfType(ComponentBlockDefinition) != null){
			var ComponentBlockDefinition block = p1.getContainerOfType(ComponentBlockDefinition)
			var ArrayList<VariableDeclaration> attributes = new ArrayList<VariableDeclaration>(block.eAllOfType(VariableDeclaration))
			output.put(block,attributes)
			
		} else {
			
			var ArrayList<Process> roots = p1.getRootProcesses
			
			for(p2 : roots){
				var ComponentStyle componentStyle = p2.getContainerOfType(ComponentStyle)
				if(componentStyle.eAllOfType(ComponentLineStyle).size > 0){
					var ArrayList<ComponentLineDefinition> lines = new ArrayList<ComponentLineDefinition>(componentStyle.eAllOfType(ComponentLineDefinition))
					for(line : lines){
						if(line.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
							var ArrayList<VariableDeclaration> attributes = new ArrayList<VariableDeclaration>(line.eAllOfType(VariableDeclaration))
							output.put(line,attributes)
						}
					}
				} else {
					var ArrayList<ComponentBlockDefinition> blocks = new ArrayList<ComponentBlockDefinition>(componentStyle.eAllOfType(ComponentBlockDefinition))
					var HashMap<String,Component> components = new HashMap<String,Component>();
					for(block : blocks){
						components.put((block.name as ComponentName).name,block)
						if(block.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
							var ArrayList<VariableDeclaration> attributes = new ArrayList<VariableDeclaration>(block.eAllOfType(VariableDeclaration))
							output.put(block,attributes)
						}
					}
				}
			}
		}
			
		return output 
	}
	
	/**
	 * Given a list of Macro's return a list of the Macro names
	 * @author CDW
	 */
	 def ArrayList<String> getNames(List<MacroExpressionReference> macroExpressions){
	 	
	 	var ArrayList<String> names = new ArrayList<String>()
	 	
	 	for(macro : macroExpressions){
	 		names.add((macro.name as MacroName).name.name)
	 	}
	 	
	 	return names
	 	
	 }
	 
	/**
	 * Given a Process, return the Processes the owning Component starts with.
	 * <p>
	 * 
	 * @author 	CDW
	 * @param	Process
	 * @return	Process
	 */
	def ArrayList<Process> getRootProcesses(Process p1){
		
		var ArrayList<String> names = new ArrayList<String>();
		var ArrayList<Process> roots = new ArrayList<Process>();
		
		//process is in behaviour block
		if(p1.getContainerOfType(ComponentBlockDefinition) != null){
			var String componentName = ""
			componentName = (p1.getContainerOfType(ComponentBlockDefinition).name as ComponentName).name
			var ArrayList<MacroExpressionReference> macros = new ArrayList<MacroExpressionReference>(p1.getContainerOfType(Model).eAllOfType(MacroExpressionReference))
			for(macro : macros){
				//can only be associated to components that have the same name
				if(macro.getComponentName.equals(componentName)){
					names.add((macro.name as MacroName).name.name)
				}
				
			}
		//process is not in behaviour block
		} else {
			var ArrayList<MacroExpressionReference> macros = new ArrayList<MacroExpressionReference>(p1.getContainerOfType(Model).eAllOfType(MacroExpressionReference))
			for(macro : macros){
				//ignore behaviour blocks
				if(macro.getContainerOfType(ComponentBlockDefinition) == null)
						//associated to anything with the process name
						names.add((macro.name as MacroName).name.name)
			}
			
		}
		
		for(Process p2 : p1.maximumFixedPoint(true))
				if(names.contains((p2.name as ProcessName).name))
					roots.add(p2)
				
//		println("roots:" + roots)
		return roots
	} 
	
	/**
	 * Given a MacroExpression, return the name of the containing component
	 */
	def String getComponentName(MacroExpressionReference macro){
		if(macro.getContainerOfType(ComponentBlockNewDeclaration) != null){
			(macro.getContainerOfType(ComponentBlockNewDeclaration).name  as ComponentName).name
		} else if (macro.getContainerOfType(ComponentBlockNewDeclarationSpawn) != null){
			(macro.getContainerOfType(ComponentBlockNewDeclarationSpawn).name  as ComponentName).name
		} else if (macro.getContainerOfType(InitBlock) != null){
			(macro.getContainerOfType(ComponentBlockDefinition).name  as ComponentName).name
		} else if (macro.getContainerOfType(ComponentLineDefinition) != null){
			(macro.getContainerOfType(ComponentLineDefinition).name  as ComponentName).name
		} else if (macro.getContainerOfType(ComponentLineDefinitionSpawn) != null){
			(macro.getContainerOfType(ComponentLineDefinitionSpawn).name  as ComponentName).name
		} else if (macro.getContainerOfType(EnvironmentMacroExpressionComponentAState) != null){
			(macro.getContainerOfType(EnvironmentMacroExpressionComponentAState).comp  as ComponentName).name
		} else {
			"no component"
		}
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
				if(p1.name.equals(pr.expression.name))
					output.add(p2)
		
		for(ProcessExpressionReference pr : p1.eAllOfType(ProcessExpressionReference))
			for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
				if(p2.name.equals(pr.expression.name))
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
				if(p1.name.equals(pr.expression.name))
					output.add(p2)
							
		return output 
		
	}
	
	/**
	 * Given an ActionStub, return all Processes which use that action
	 */
	def ArrayList<Process> getProcesses(ActionStub stub){
	 	
		var actions = stub.getContainerOfType(Model).eAllOfType(Action)
	 	var output = new ArrayList<Process>()
	 	
	 	for(action : actions)
	 		if(action.name.equals(stub.name))
	 			output.add(action.getContainerOfType(Process))
	 	
		return output
	 	
	}
	
	/**
	 * Given a Component return the state Tree
	 */
	def Tree getTree(Component component){
		
		var HashSet<Process> processes = new HashSet<Process>()
		var Tree tree = null;
		
		if(component.getContainerOfType(ComponentBlockStyle) != null){
			
			var ms = (component as ComponentBlockDefinition).getComponentBlockDeclarations.stripMacro
			
			processes.addAll(component.getProcess(ms))
			processes.addAll(getProcess(ms))
			processes.addAll((component as ComponentBlockDefinition).eAllOfType(Process))
			
			for(p : processes)
				processes.addAll(p.allReferences)
			
		} else {
			
			var ms = new ArrayList<MacroName>((component as ComponentLineDefinition).eAllOfType(MacroName))
			processes.addAll(component.getProcess(ms))
			processes.addAll(getProcess(ms))
			
			for(p : processes)
				processes.addAll(p.allReferences)
			
		}
		
		if(processes.size > 0){
			tree = new Tree(processes);
		}
		
		return tree
	}
	
	/**
	 * Given a ProcessExpression, return the type
	 */
	def String whatProcessExpression(ProcessExpression pe){
		switch(pe){
			ProcessExpressionChoice: 	"c"
			ProcessExpressionLeaf:		"l"
			ProcessExpressionGuard:		"g"
			ProcessExpressionAction: 	"a"
			ProcessExpressionReference: "r"
		}
	}
	
	/**
	 * Given a model, return each component name with it's variables and their type
	 */
	def HashMap<Component, ArrayList<VariableDeclaration>> getComponentAttributeType(Model model){
		
		var	HashMap<Component, ArrayList<VariableDeclaration>> output = new HashMap<Component, ArrayList<VariableDeclaration>>()
		
		var components = model.eAllOfType(Component)
		
		for(component : components){
			output.put(component,new ArrayList<VariableDeclaration>(component.eAllOfType(VariableDeclaration)))
		}
		
		return output
		
	}
	
	/**
	 * Given a ComponentBlockDefinition, return all declarations
	 */
	def ArrayList<CBND> getComponentBlockDeclarations(ComponentBlockDefinition cbd){
		var cbndss = cbd.getContainerOfType(Model).eAllOfType(ComponentBlockNewDeclarationSpawn)
		var cbnds = cbd.getContainerOfType(Model).eAllOfType(ComponentBlockNewDeclaration)
		var ArrayList<CBND> output = new ArrayList<CBND>()
		var name = cbd.name
		
		for(cbnd : cbndss){
			if(cbnd.name.equals(name))
				output.add(cbnd)
		}
		
		for(cbnd : cbnds){
			if(cbnd.name.equals(name))
				output.add(cbnd)
		}
		
		return output
	}
	
	/**
	 * Given an arraylist of CBND, return a list of MacroName
	 */
	def ArrayList<MacroName> stripMacro(ArrayList<CBND> cbnds){
		
		var ArrayList<MacroName> output = new ArrayList<MacroName>()
		
		for(cbnd : cbnds){
			output.addAll(cbnd.eAllOfType(MacroName))
		}
		
		return output
		
	}
	
	/**
	 * Given an arraylist of CBND, return a list of MacroExpressionReference
	 */
	def ArrayList<MacroExpressionReference> stripMacroExpressionReference(ArrayList<CBND> cbnds){
		
		var ArrayList<MacroExpressionReference> output = new ArrayList<MacroExpressionReference>()
		
		for(cbnd : cbnds){
			output.addAll(cbnd.eAllOfType(MacroExpressionReference))
		}
		
		return output
		
	}
	
	/**
	 * Given a list of MacroNames and a component, return the Processes
	 */
	def ArrayList<Process> getProcess(Component component, ArrayList<MacroName> macros){
		
		var ArrayList<Process> output = new ArrayList<Process>()
		
		if(macros.size > 0)
			for(process : component.eAllOfType(Process)){
				for(macroName : macros){
					if(process.name.equals(macroName.name)){
						output.add(process)
					}
				}
			}
		
		return output
		
	}
	
	/**
	 * Given a list of MacroNames return the associated Processes found in the Process block
	 */
	def ArrayList<Process> getProcess(ArrayList<MacroName> macros){
		
		var ArrayList<Process> output = new ArrayList<Process>()
		
		if(macros.size > 0){
			if(macros.get(0).getContainerOfType(Model).eAllOfType(Processes).size > 0){
				var processes = macros.get(0).getContainerOfType(Model).eAllOfType(Processes).get(0).eAllOfType(Process)
				for(process : processes){
					for(macroName : macros){
						if(process.name.equals(macroName.name)){
							output.add(process)
						}
					}
				}
			}
		}
		
		return output
	}
	
	/**
	 * Given a process, return all possible associated Actions
	 */
	def HashSet<Action> getActionsFromProcess(Process process){
		var HashSet<Action> output = new HashSet<Action>()
		
		var HashSet<Process> processes = getAllReferences(process)
		
		for(p : processes){
			output.addAll(p.eAllOfType(Action))
		}
		
		return output
	}
	
	/**
	 * Given a component, return a list of all Actions associated
	 */
	def ArrayList<Action> getActionsFromComponent(Component component){
		var ArrayList<Action> output = new ArrayList<Action>()
		
		if(component.getContainerOfType(ComponentBlockStyle) != null){
			
			var ms = (component as ComponentBlockDefinition).getComponentBlockDeclarations.stripMacro
			
			var p1s = component.getProcess(ms)
			var p2s = getProcess(ms)
			
			for(p : p1s){
				output.addAll(p.getActionsFromProcess)
			}
			
			for(p : p2s){
				output.addAll(p.getActionsFromProcess)
			}
			
			output.addAll((component as ComponentBlockDefinition).eAllOfType(Action))
			
		} else {
			
			var ms = new ArrayList<MacroName>((component as ComponentLineDefinition).eAllOfType(MacroName))
			var p1s = component.getProcess(ms)
			var p2s = getProcess(ms)
			
			for(p : p1s){
				output.addAll(p.getActionsFromProcess)
			}
			
			for(p : p2s){
				output.addAll(p.getActionsFromProcess)
			}
			
		}
		
		return output
	}
	
//	/**
//	 * Given an action return if it is an output
//	 */
//	def boolean isOutput(Action action){
//		action.eAllOfType(OutputAction).size > 0 || action.eAllOfType(SpontaneousAction).size > 0
//	}
//	
//	/**
//	 * Given an action return if it is a multicast
//	 */
//	def boolean isMulticast(Action action){
//		action.eAllOfType(MultiCast).size > 0 
//	}
	
	/**
	 * Given a component name, and an attribute name, return the value of the attribute
	 */
	def String getValue(Model model, String componentName, String attributeName){
		var String output = ""
		var component = componentName.getComponent(model)
		var attributes = component.eAllOfType(VariableDeclaration)
		
		for(attribute : attributes){
			if(attribute.name.label.equals(attributeName))
				output = attribute.label
		}
		
		return output
	}
	
	/**
	 * Given a component name, and an attribute name, return the value of the attribute
	 */
	def String getValue(Model model, String componentName, String attributeName, String recordName){
		var String output = ""
		var component = componentName.getComponent(model)
		var attributes = component.eAllOfType(VariableDeclaration)
		
		for(attribute : attributes){
			if(attribute.name.label.equals(attributeName)){
				var rds = attribute.eAllOfType(RecordDeclaration)
					for(rd : rds){
						if(rd.name.label.equals(recordName))
							output = rd.assign.label
					}
				
			}
				
				
		}
		
		return output
	}
	
	def String convertType(VariableDeclaration vd){
		if(vd.type.toString.equals("enum")){
			return "Integer"
		}
		if(vd.type.toString.equals("record")){
			return "Integer"
		}
		if(vd.type.toString.equals("double")){
			return "Double"
		}
		if(vd.type.toString.equals("integer")){
			return "Integer"
		}
	}
	
	def String convertPrimitiveType(VariableDeclaration vd){
		if(vd.type.toString.equals("enum")){
			return "int"
		}
		if(vd.type.toString.equals("record")){
			return "int"
		}
		if(vd.type.toString.equals("double")){
			return "double"
		}
		if(vd.type.toString.equals("integer")){
			return "int"
		}
	}
	
	/**
	 * Given a variable reference, find variabledeclaration from COMPONENT!
	 */
	def VariableDeclaration getVariableDeclaration(VariableReference vrr){
		var components = new ArrayList<Component>(vrr.getContainerOfType(Model).eAllOfType(Component))
		components.getVariableDeclaration(vrr.name)
	}
	
	/**
	 * Given a variable reference, find variabledeclaration from COMPONENT!
	 */
	def VariableDeclaration getVariableDeclaration(Name name){
		var components = new ArrayList<Component>(name.getContainerOfType(Model).eAllOfType(Component))
		components.getVariableDeclaration(name)
	}
	
	def VariableDeclaration getVariableDeclaration(ArrayList<Component> components, Name name){
		var VariableDeclaration output = null
		for(component : components){
			for(variableDeclaration : component.eAllOfType(VariableDeclaration))
				if(name.sameName(variableDeclaration.name))
					output = variableDeclaration
		}
		return output
	}
	
	def boolean isRecord(Name name){
		var vd = getVariableDeclaration(name)
		switch(vd){
			VariableDeclarationRecord: true
			default: false
		}
	}
	
	def boolean isEnum(Name name){
		var vd = getVariableDeclaration(name)
		switch(vd){
			VariableDeclarationEnum: true
			default: false
		}
	}
	
	/**
	 * Given a VariableDeclaration return all VariableDeclarations with the same name
	 */
	def ArrayList<VariableDeclaration> getSameDeclarations(VariableDeclaration vd){
		var ArrayList<VariableDeclaration> output = new ArrayList<VariableDeclaration>()
		
		for(vardec : vd.getContainerOfType(Model).eAllOfType(VariableDeclaration)){
			if(vardec.name.sameName(vd.name))
				output.add(vardec)
		}
		
		return output
	}
	
	/**
	 * Given a VariableDeclaration return boolean if all VariableDeclarations in the model
	 * are the same type
	 */
	def boolean sameType(VariableDeclaration vd){
		var others = vd.sameDeclarations
		var boolean output = true
		
		for(o : others){
			output = output && vd.type.toString.equals(o.type.toString)
		}
		
		return output
	}
	
	/**
	 * Is this a BlockSystem?
	 */
	def boolean isBlockSystem(System system){
		switch(system){
			BlockSystem: true
			LineSystem:	false
		}
	}
	
	/**
	 * Given a CBND return if ComponentName is found in same Model
	 */
	def boolean isInModel(CBND cbnd){
		var boolean output = false
		var name = cbnd.name
		
		for(component : cbnd.getContainerOfType(Model).eAllOfType(ComponentBlockDefinition))
			output = component.name.sameName(name) || output
		
		return output
	}
	
	/**
	 * Given a CBND return ComponentBlockDefinition
	 */
	def ComponentBlockDefinition getComponent(CBND cbnd){
		var ComponentBlockDefinition output = null
		var name = cbnd.name
		
		for(component : cbnd.getContainerOfType(Model).eAllOfType(ComponentBlockDefinition))
			if(component.name.sameName(name))
				output = component
		
		return output
	}
	
	/**
	 * Given a CBND check argument count and type of matching ComponentBlockDefinition
	 */
	def boolean hasMatchingArguments(CBND cbnd){
		var cbndArguments = cbnd.eAllOfType(NCA)
		var componentArguments = cbnd.component.eAllOfType(ComponentArgument)
		if(cbndArguments.size != componentArguments.size)
			return false
		else{
			var output = true
			var count = 0
			for(;count < cbndArguments.size;count++){
				output = output && cbndArguments.get(count).type.toString.equals(componentArguments.get(count).type.toString)
			}
			
			return output
		}	
	}
	
	/**
	 * Given a CBND check argument count and type of matching ComponentBlockDefinition
	 */
	def boolean hasMatchingArguments(CBND cbnd, ComponentBlockDefinition cbd){
		var cbndArguments = cbnd.eAllOfType(NCA)
		var componentArguments = cbd.eAllOfType(ComponentArgument)
		if(cbndArguments.size != componentArguments.size)
			return false
		else{
			var output = true
			var count = 0
			for(;count < cbndArguments.size;count++){
				output = output && cbndArguments.get(count).type.toString.equals(componentArguments.get(count).type.toString)
			}
			
			return output
		}	
	}
	
	/**
	 * Given a component return all declarations
	 */
	def ArrayList<CBND> getCBNDs(ComponentBlockDefinition cbd){
		var ArrayList<CBND> output = new ArrayList<CBND>()
		
		for(cbnd : cbd.getContainerOfType(Model).eAllOfType(CBND))
			if(cbd.name.sameName(cbnd.name))
				if(cbnd.hasMatchingArguments(cbd))
					output.add(cbnd)
		
		return output
	}
	
	def HashMap<String,String> getNameValue(ArrayList<VariableName> vns){
		
		var HashMap<String,String> output = new HashMap<String,String>()
		
		for(vn : vns){
			output.putAll(vn.nameValueLabel)
		}
		
		return output
		
	}
	
	/**
	 * Given a Component get startingStates
	 */
	def ArrayList<String> getStartingStates(Component component){
		var ArrayList<String> startStates = new ArrayList<String>()
		
		
		return startStates
	}
	
	/**
	 * Given a Component get MacroExpressionReference
	 */
	def ArrayList<MacroExpressionReference> getMacros(Component component){
		
		switch(component){
			ComponentBlockDefinition: 	return (component as ComponentBlockDefinition).getMacrosBlock
			default:					return (component as ComponentLineDefinition ).getMacrosLine
		}
		
		
	}
	
	def ArrayList<MacroExpressionReference> getMacrosBlock(ComponentBlockDefinition component){
		var ArrayList<MacroExpressionReference> macros = new ArrayList<MacroExpressionReference>()
		
		var ms = component.getComponentBlockDeclarations.stripMacroExpressionReference
		var inits = component.eAllOfType(MacroExpressionReference)
		var processes = component.getContainerOfType(Model).eAllOfType(Process)
		
		for(p : processes){
			for(m : ms)
				if(p.name.sameName(m.name))
					macros.add(m)
			for(i : inits)
				if(p.name.sameName(i.name))
					macros.add(i)
		}
		
		return macros
	}
	
	def ArrayList<MacroExpressionReference> getMacrosLine(ComponentLineDefinition component){
		new ArrayList<MacroExpressionReference>(component.eAllOfType(MacroExpressionReference))
	}
	
//	/**
//	 * Given a ProcessExpressionReference return the Process
//	 */
//	def Process getReferredProcess(ProcessExpressionReference per){
//		var Process output = null
//		
//		var processBlock = per.getContainerOfType(Process)
//		if(processBlock != null){
//			for(process : processBlock.eAllOfType(Process)){
//				if(process.name.sameName(per.expression)){
//					output = process
//				}
//			}
//		}
//		
//		if(output != null){
//			var processes = per.getContainerOfType(Processes)
//			if(processes != null){
//				for(process : processes.eAllOfType(Process)){
//					if(process.name.sameName(per.expression)){
//						output = process
//					}
//				}
//			}
//		}
//		return output
//	}
	 
}