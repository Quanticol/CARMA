package eu.quanticol.carma.core.utils

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Name

class Util {
	
	@Inject extension LabelUtil
	
	def boolean sameName(Name name1, Name name2){
		name1.name.equals(name2.name)
	}
	
//	def Name getName(Component c){
//		return c.eAllOfType(ComponentSignature).get(0).eAllOfType(Name).get(0)
//	}
//	
//	def Name getName(Parameters parameter){
//		return parameter.eAllOfType(Name).get(0)
//	}
//	
//	def Name getName(CompParameters parameter){
//		return parameter.eAllOfType(Name).get(0)
//	}
//	
//	def Name getName(MethodDeclaration md){
//		return md.eAllOfType(Name).get(0)
//	}
//	
//	def Name getName(StoreDeclaration sd){
//		return sd.eAllOfType(Name).get(0)
//	}
//	
//	def Name getRecordName(VariableReference vr){
//		if(vr.eAllOfType(RecordName).size > 0)
//			return vr.eAllOfType(RecordName).get(0)
//		else
//			return null
//	}
//	
//	def Name getVariableName(VariableReference vr){
//		if(vr.eAllOfType(VariableName).size > 0)
//			return vr.eAllOfType(VariableName).get(0)
//		else
//			return null
//	}
//	
//	def Name getRootName(VariableReference vr){
//		if(vr.recordName == null)
//			return vr.variableName
//		else
//			return vr.recordName
//	}
//	
//	def ArrayList<Process> getProcesses(ActionStub stub, boolean output){
//	 	
//		var actions = stub.getContainerOfType(Model).eAllOfType(Action)
//	 	var processes = new ArrayList<Process>()
//	 	
//	 	for(action : actions){
//	 		if(action.name.sameName(stub.name)){
//	 			if(output && action.isOutput)
//	 					processes.add(action.getContainerOfType(Process))
//	 			if(!output && !action.isOutput)
//	 					processes.add(action.getContainerOfType(Process))
//	 		}
//	 	}
//		return processes
//	 	
//	}
//	
//	def boolean isOutput(Action action){
//		action.eAllOfType(OutputAction).size > 0 || action.eAllOfType(SpontaneousAction).size > 0
//	}
//	
//	def void getComponents(EnvironmentMacroExpressions eme, ArrayList<Component> components){
//		switch(eme){
//			EnvironmentMacroExpressionParallel:				{
//				eme.left.getComponents(components)
//				eme.right.getComponents(components)
//				}
//			EnvironmentMacroExpressionAll:					{
//				components.addAll(eme.getContainerOfType(Model).eAllOfType(Component))
//			}
//			EnvironmentMacroExpressionComponentAllStates: {
//				components.add((eme as EnvironmentMacroExpressionComponentAllStates).comp.getContainerOfType(Component))
//			}
//			EnvironmentMacroExpressionComponentAState:	{
//				components.add((eme as EnvironmentMacroExpressionComponentAState).comp.getContainerOfType(Component))
//			}
//		}
//		
//	}
//	
//	def ArrayList<Component> getComponents(Process proc){
//		
//		var output = new ArrayList<Component>
//		
//		if(proc.getContainerOfType(ComponentBlockDefinition) != null){
//			output.add(proc.getContainerOfType(ComponentBlockDefinition))
//		} else {
//			var ArrayList<Process> roots = proc.getRootProcesses
//			for(p2 : roots){
//				var ComponentStyle componentStyle = p2.getContainerOfType(ComponentStyle)
//				if(componentStyle.eAllOfType(LineStyle).size > 0){
//					var lines = componentStyle.eAllOfType(ComponentLineDefinition)
//					for(line : lines){
//						if(line.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
//							output.add(line)
//						}
//					}
//					var spawns = componentStyle.eAllOfType(ComponentLineDefinitionSpawn)
//					for(line : spawns){
//						if(line.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
//							output.add(line)
//						}
//					}
//				} else {
//					var news = componentStyle.eAllOfType(ComponentBlockNew)
//					var Component component = null
//					for(n : news){
//						if(n.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
//							component = n.getComponent
//							output.add(component)
//						}
//					}
//					var spawns = componentStyle.eAllOfType(ComponentBlockSpawn)
//					for(s : spawns){
//						if(s.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
//							component = s.getComponent
//							output.add(component)
//						}
//					}
//				}
//			}
//		}
//			
//		return output 
//	}
//	
//	def ArrayList<Process> getRootProcesses(Process p1){
//		
//		var ArrayList<Name> names = new ArrayList<Name>();
//		var ArrayList<Process> roots = new ArrayList<Process>();
//		
//		//process is in behaviour block
//		if(p1.getContainerOfType(ComponentBlockDefinition) != null){
//			var component = p1.getContainerOfType(Component)
//			var macros = component.eAllOfType(MacroExpressionReference)
//			for(macro : macros){
//					names.add(macro.name)
//			}
//		//process is not in behaviour block
//		} else {
//			var macros = p1.getContainerOfType(Model).eAllOfType(MacroExpressionReference)
//			for(macro : macros){
//				//ignore component-behaviour blocks, only check new/spawn 
//				if(macro.getContainerOfType(ComponentBlockDefinition) == null)
//						//associated to anything with the process name
//						names.add(macro.name)
//			}
//		}
//		for(Process p2 : p1.maximumFixedPoint(true))
//			for(name : names){
//				if(name.sameName(p2.name))
//					roots.add(p2)
//			}
//		return roots
//	}
//	
//	def ArrayList<String> getNames(List<MacroExpressionReference> macroExpressions){
//		var ArrayList<String> names = new ArrayList<String>()
//		for(macro : macroExpressions){
//			names.add((macro.name as MacroName).name.name)
//		}
//		return names
//	}
//
//	def Component getComponent(ComponentBlockNew cbn){
//		var components = cbn.getContainerOfType(Model).eAllOfType(Component)
//		for(component : components)
//			if(cbn.name.sameName(component.getName))
//				return component
//	}
//	
//	def Component getComponent(ComponentBlockSpawn cbn){
//		var components = cbn.getContainerOfType(Model).eAllOfType(Component)
//		for(component : components)
//			if(cbn.name.sameName(component.getName))
//				return component
//	}
//	
//	def boolean isNameInModel(Model model, Name name){
//		var test = false
//		var names = model.eAllOfType(Name)
//		
//		for(n:names){
//			test = test || n.sameName(name)
//		}
//		
//		return test
//	}
//	
//	def ArrayList<Name> getNames(ProcessesBlock psb){
//		var ArrayList<Name> names = new ArrayList<Name>()
//			if(psb.processes != null){
//				for(p : psb.processes)
//					names.add(p.name)
//			}
//		return names
//	}
//	
//	def ArrayList<Name> getNames(Processes ps){
//		var ArrayList<Name> names = new ArrayList<Name>()
//			if(ps.processes != null){
//				for(p : ps.processes)
//					names.add(p.name)
//			}
//		return names
//	}
//	
//	def ArrayList<Name> getNames(StoreBlock sb){
//		var ArrayList<Name> names = new ArrayList<Name>()
//			if(sb.attributes != null){
//				for(p : sb.attributes)
//					names.add(p.name)
//			}
//		return names
//	}
//	
//	def ArrayList<Name> getNames(StoreLine sl){
//		var ArrayList<Name> names = new ArrayList<Name>()
//			if(sl.attributes != null){
//				for(p : sl.attributes)
//					names.add(p.name)
//			}
//		return names
//	}
//	
//	def ArrayList<Name> getNames(Methods ms){
//		var ArrayList<Name> names = new ArrayList<Name>()
//			if(ms.methods != null){
//				for(p : ms.methods)
//					names.add(p.name)
//			}
//		return names
//	}
//	
//	def ArrayList<OutputAction> getSender(InputAction ia){
//		var ArrayList<OutputAction> output = new ArrayList<OutputAction>()
//		for(oa : ia.getContainerOfType(Model).eAllOfType(OutputAction))
//			if(oa.getContainerOfType(Action).name.sameName(ia.getContainerOfType(Action).name))
//				output.add(oa)
//		return output
//	}
//	
//	def ArrayList<InputAction> getReceiver(OutputAction oa){
//		var ArrayList<InputAction> output = new ArrayList<InputAction>()
//		for(ia : oa.getContainerOfType(Model).eAllOfType(InputAction))
//			if((ia.getContainerOfType(Action).name as ActionName).name.equals((oa.getContainerOfType(Action).name as ActionName).name))
//				output.add(ia)
//		return output
//	}
//	
//	def ArrayList<Process> maximumFixedPoint(Process p1, boolean includeSelf){
//		
//		
//		var HashSet<Process> buffer1 = new HashSet<Process>()
//		var HashSet<Process> buffer2 = new HashSet<Process>()
//		
//		if(includeSelf)
//			buffer1.add(p1)
//		else
//			buffer1.addAll(getReferences(p1))
//		
//		while(buffer1.size > buffer2.size){
//			
//			buffer2.addAll(buffer1)
//			
//			for(Process p2 : buffer2){
//				if(includeSelf)
//					buffer1.addAll(getAllReferences(p2))
//				else
//					buffer1.addAll(getReferences(p2))
//			}
//		}
//		
//		var ArrayList<Process> output = new ArrayList<Process>(buffer1)
//		
//		return output
//		
//	}
//	
//	def HashSet<Process> getAllReferences(Process p1){
//		
//		var HashSet<Process> output = new HashSet<Process>()
//		
//		for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
//			for(ProcessExpressionReference pr : p2.eAllOfType(ProcessExpressionReference))
//				if(p1.name.equals(pr.expression.name))
//					output.add(p2)
//		
//		for(ProcessExpressionReference pr : p1.eAllOfType(ProcessExpressionReference))
//			for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
//				if(p2.name.equals(pr.expression.name))
//					output.add(p2)
//					
//		return output 
//		
//	}
//	
//	def HashSet<Process> getReferences(Process p1){
//		
//		var HashSet<Process> output = new HashSet<Process>()
//		
//		for(Process p2 : p1.getContainerOfType(ComponentStyle).eAllOfType(Process))
//			for(ProcessExpressionReference pr : p2.eAllOfType(ProcessExpressionReference))
//				if(p1.name.equals(pr.expression.name))
//					output.add(p2)
//							
//		return output 
//		
//	}
//	
//	def boolean isBroadcast(ActionStub actionStub){
//		return actionStub.label.contains("*")
//	}
//
//	
	//////here be dragons
//	def ArrayList<Action> getActions(ActionStub actionStub){
//		var output = new ArrayList<Action>()
//		for(proc : actionStub.processes){
//			output.addAll(proc.eAllOfType(Action))
//		}
//		return output
//	}
//		
//	def ArrayList<Process> getProcesses(ActionStub stub){
//	 	
//		var actions = stub.getContainerOfType(Model).eAllOfType(Action)
//	 	var output = new ArrayList<Process>()
//	 	
//	 	for(action : actions){
//	 		if(action.name.sameName(stub.name)){
//	 			output.add(action.getContainerOfType(Process))
//	 		}
//	 	}
//		return output
//	 	
//	}
//	def HashSet<String> getTypes(VariableReference vr, OutputAction oa){
//		var HashSet<String> output = new HashSet<String>()
//		if(vr.getContainerOfType(Action) != null){
//			var ArrayList<Action> actions = new ArrayList<Action>()
//			var action = vr.getContainerOfType(Action)
//			actions.addAll(action.getOpposite)
//			for(a : actions){
//				var cad = a.getContainerOfType(Process).componentAndDeclarations
//				for(component : cad.keySet){
//					output.addAll(vr.name.getTypesVD(cad.get(component)))
//				}
//			}
//		}
//		return output
//	}
//				
//
//	/**
//	 * @see InputActionArguments
//	 * needs to know what position it is in the argument list
//	 * the output argument should not be the same name
//	 */
//	def HashSet<String> getTypes(VariableName vn, InputAction ia){
//		var HashSet<String> output = new HashSet<String>()
//		var action = ia.getContainerOfType(Action)
//		var args = ia.inputActionArguments.inputArguments
//		var count = 0
//		var index = 0
//		
//		if(action != null){
//			for(arg : args){
//				if((arg as VariableName).sameName(vn))
//					index = count
//				else
//					count++
//			}
//			
//			if(args.size > 0){
//				for(oas : ia.getSender){
//					if(oas.outputActionArguments.outputArguments.size > index){
//						if(oas.outputActionArguments != null){
//							output.add((oas.outputActionArguments.outputArguments.get(index) as OutputActionArgument).type.toString)
//						}
//						else
//							output.add("null")
//					} else {
//						output.add("null")
//					}
//				}
//			}
//		}
//		return output
//	}
//	 
//	def HashSet<String> getTypes(VariableReference vr, InputAction ia){
//		var HashSet<String> output = new HashSet<String>()
//		var vns = ia.eAllOfType(VariableName)
//		for(vn : vns){
//			if(vn.sameName(vr.name))
//				output.addAll(vr.name.getTypes(ia))
//		}
//		return output
//	}
//	
//	/** 
//	 * @see ComponentBlockForStatement
//	 * 
//	 */
//	def HashSet<String> getTypes(VariableName vn, ComponentBlockForStatement cbfs){
//		var HashSet<String> output = new HashSet<String>()
//		if(cbfs != null ){
//			output.addAll(vn.getTypesVD(
//				new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentBlockForStatement).eAllOfType(VariableDeclaration))
//			))
//		}
//		return output
//	}
//	
//	/** 
//	 * @see ComponentBlockForStatement
//	 * 
//	 */
//	def HashSet<String> getTypes(VariableName vn,ComponentLineForStatement clfs){
//		var HashSet<String> output = new HashSet<String>()
//		if(clfs != null ){
//			output.addAll(vn.getTypesVD(
//				new ArrayList<VariableDeclaration>(vn.getContainerOfType(ComponentLineForStatement).eAllOfType(VariableDeclaration))
//			))
//		}
//		return output
//	}
//	
//
//	
//	/**
//	 * Return list of types that have this variable name
//	 * @author CDW
//	 */
//	def ArrayList<String> getTypesVD(Name variableName, ArrayList<VariableDeclaration> variables){
//		var ArrayList<String> output = new ArrayList<String>()
//		for(variable : variables)
//			if(variable.name.sameName(variableName))
//				output.add(variable.type)
//		
//		return output
//	}
//	
//	def ArrayList<VariableDeclaration> getEnvironmentAttributes(Model model){
//		return new ArrayList<VariableDeclaration>(model.eAllOfType(Environment).get(0).eAllOfType(VariableDeclaration))
//	}
//	
//	/**
//	 * Return the components with attributes related to this Process 
//	 * 
//	 * @author 	CDW
//	 * @param	Process
//	 * @return	HashMap<Integer,ArrayList<String>>
//	 */
//	def HashMap<Component,ArrayList<VariableDeclaration>> getComponentAndDeclarations(Process p1){
//		
//		var output = new HashMap<Component,ArrayList<VariableDeclaration>>
//		
//		if(p1.getContainerOfType(ComponentBlockDefinition) != null){
//			var ComponentBlockDefinition block = p1.getContainerOfType(ComponentBlockDefinition)
//			var ArrayList<VariableDeclaration> attributes = new ArrayList<VariableDeclaration>(block.eAllOfType(VariableDeclaration))
//			output.put(block,attributes)
//			
//		} else {
//			
//			var ArrayList<Process> roots = p1.getRootProcesses
//			
//			for(p2 : roots){
//				var ComponentStyle componentStyle = p2.getContainerOfType(ComponentStyle)
//				if(componentStyle.eAllOfType(LineStyle).size > 0){
//					var ArrayList<ComponentLineDefinition> lines = new ArrayList<ComponentLineDefinition>(componentStyle.eAllOfType(ComponentLineDefinition))
//					for(line : lines){
//						if(line.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
//							var ArrayList<VariableDeclaration> attributes = new ArrayList<VariableDeclaration>(line.eAllOfType(VariableDeclaration))
//							output.put(line,attributes)
//						}
//					}
//				} else {
//					var ArrayList<ComponentBlockDefinition> blocks = new ArrayList<ComponentBlockDefinition>(componentStyle.eAllOfType(ComponentBlockDefinition))
//					var HashMap<String,Component> components = new HashMap<String,Component>();
//					for(block : blocks){
//						components.put(block.eAllOfType(ComponentName).get(0).name,block)
//						if(block.eAllOfType(MacroExpressionReference).getNames().contains((p2.name as ProcessName).name)){
//							var ArrayList<VariableDeclaration> attributes = new ArrayList<VariableDeclaration>(block.eAllOfType(VariableDeclaration))
//							output.put(block,attributes)
//						}
//					}
//				}
//			}
//		}
//			
//		return output 
//	}
//	
//	
//	/**
//	 * Given a MacroExpression, return the name of the containing component
//	 */
//	def String getComponentName(MacroExpressionReference macro){
//		if(macro.getContainerOfType(ComponentBlockNew) != null){
//			(macro.getContainerOfType(ComponentBlockNew).name  as ComponentName).name
//		} else if (macro.getContainerOfType(ComponentBlockSpawn) != null){
//			(macro.getContainerOfType(ComponentBlockSpawn).name  as ComponentName).name
//		} else if (macro.getContainerOfType(InitBlock) != null){
//			macro.getContainerOfType(ComponentBlockDefinition).eAllOfType(ComponentName).get(0).name
//		} else if (macro.getContainerOfType(ComponentLineDefinition) != null){
//			(macro.getContainerOfType(ComponentLineDefinition).name  as ComponentName).name
//		} else if (macro.getContainerOfType(ComponentLineDefinitionSpawn) != null){
//			(macro.getContainerOfType(ComponentLineDefinitionSpawn).name  as ComponentName).name
//		} else if (macro.getContainerOfType(EnvironmentMacroExpressionComponentAState) != null){
//			(macro.getContainerOfType(EnvironmentMacroExpressionComponentAState).comp  as ComponentName).name
//		} else {
//			"no component"
//		}
//	}
//	
//	/**
//	 * Given a Component return the state Tree
//	 */
//	def Tree getTree(Component component){
//		
//		var HashSet<Process> processes = new HashSet<Process>()
//		var Tree tree = null;
//		
//		if(component.getContainerOfType(BlockStyle) != null){
//			
//			var ms = (component as ComponentBlockDefinition).getComponentBlockDeclarations.stripMacro
//			
//			processes.addAll(component.getProcess(ms))
//			processes.addAll(getProcess(ms))
//			processes.addAll((component as ComponentBlockDefinition).eAllOfType(Process))
//			
//			for(p : processes)
//				processes.addAll(p.allReferences)
//			
//		} else {
//			
//			var ms = new ArrayList<MacroName>((component as ComponentLineDefinition).eAllOfType(MacroName))
//			processes.addAll(component.getProcess(ms))
//			processes.addAll(getProcess(ms))
//			
//			for(p : processes)
//				processes.addAll(p.allReferences)
//			
//		}
//		
//		if(processes.size > 0){
//			tree = new Tree(processes);
//		}
//		
//		return tree
//	}
//	
//	/**
//	 * Given a model, return each component name with it's variables and their type
//	 */
//	def HashMap<Component, ArrayList<VariableDeclaration>> getComponentAttributeType(Model model){
//		
//		var	HashMap<Component, ArrayList<VariableDeclaration>> output = new HashMap<Component, ArrayList<VariableDeclaration>>()
//		
//		var components = model.eAllOfType(Component)
//		
//		for(component : components){
//			output.put(component,new ArrayList<VariableDeclaration>(component.eAllOfType(VariableDeclaration)))
//		}
//		
//		return output
//		
//	}
//	
//	/**
//	 * Given a ComponentBlockDefinition, return all declarations
//	 */
//	def ArrayList<CBND> getComponentBlockDeclarations(ComponentBlockDefinition cbd){
//		var cbndss = cbd.getContainerOfType(Model).eAllOfType(ComponentBlockSpawn)
//		var cbnds = cbd.getContainerOfType(Model).eAllOfType(ComponentBlockNew)
//		var ArrayList<CBND> output = new ArrayList<CBND>()
//		var name = cbd.eAllOfType(ComponentName).get(0)
//		
//		for(cbnd : cbndss){
//			if(cbnd.name.equals(name))
//				output.add(cbnd)
//		}
//		
//		for(cbnd : cbnds){
//			if(cbnd.name.equals(name))
//				output.add(cbnd)
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Given an arraylist of CBND, return a list of MacroName
//	 */
//	def ArrayList<MacroName> stripMacro(ArrayList<CBND> cbnds){
//		
//		var ArrayList<MacroName> output = new ArrayList<MacroName>()
//		
//		for(cbnd : cbnds){
//			output.addAll(cbnd.eAllOfType(MacroName))
//		}
//		
//		return output
//		
//	}
//	
//	/**
//	 * Given an arraylist of CBND, return a list of MacroExpressionReference
//	 */
//	def ArrayList<MacroExpressionReference> stripMacroExpressionReference(ArrayList<CBND> cbnds){
//		
//		var ArrayList<MacroExpressionReference> output = new ArrayList<MacroExpressionReference>()
//		
//		for(cbnd : cbnds){
//			output.addAll(cbnd.eAllOfType(MacroExpressionReference))
//		}
//		
//		return output
//		
//	}
//	
//	/**
//	 * Given a list of MacroNames and a component, return the Processes
//	 */
//	def ArrayList<Process> getProcess(Component component, ArrayList<MacroName> macros){
//		
//		var ArrayList<Process> output = new ArrayList<Process>()
//		
//		if(macros.size > 0)
//			for(process : component.eAllOfType(Process)){
//				for(macroName : macros){
//					if(process.name.equals(macroName.name)){
//						output.add(process)
//					}
//				}
//			}
//		
//		return output
//		
//	}
//	
//	/**
//	 * Given a list of MacroNames return the associated Processes found in the Process block
//	 */
//	def ArrayList<Process> getProcess(ArrayList<MacroName> macros){
//		
//		var ArrayList<Process> output = new ArrayList<Process>()
//		
//		if(macros.size > 0){
//			if(macros.get(0).getContainerOfType(Model).eAllOfType(Processes).size > 0){
//				var processes = macros.get(0).getContainerOfType(Model).eAllOfType(Processes).get(0).eAllOfType(Process)
//				for(process : processes){
//					for(macroName : macros){
//						if(process.name.equals(macroName.name)){
//							output.add(process)
//						}
//					}
//				}
//			}
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Given a process, return all possible associated Actions
//	 */
//	def HashSet<Action> getActionsFromProcess(Process process){
//		var HashSet<Action> output = new HashSet<Action>()
//		
//		var HashSet<Process> processes = getAllReferences(process)
//		
//		for(p : processes){
//			output.addAll(p.eAllOfType(Action))
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Given a component, return a list of all Actions associated
//	 */
//	def ArrayList<Action> getActionsFromComponent(Component component){
//		var ArrayList<Action> output = new ArrayList<Action>()
//		
//		if(component.getContainerOfType(BlockStyle) != null){
//			
//			var ms = (component as ComponentBlockDefinition).getComponentBlockDeclarations.stripMacro
//			
//			var p1s = component.getProcess(ms)
//			var p2s = getProcess(ms)
//			
//			for(p : p1s){
//				output.addAll(p.getActionsFromProcess)
//			}
//			
//			for(p : p2s){
//				output.addAll(p.getActionsFromProcess)
//			}
//			
//			output.addAll((component as ComponentBlockDefinition).eAllOfType(Action))
//			
//		} else {
//			
//			var ms = new ArrayList<MacroName>((component as ComponentLineDefinition).eAllOfType(MacroName))
//			var p1s = component.getProcess(ms)
//			var p2s = getProcess(ms)
//			
//			for(p : p1s){
//				output.addAll(p.getActionsFromProcess)
//			}
//			
//			for(p : p2s){
//				output.addAll(p.getActionsFromProcess)
//			}
//			
//		}
//		
//		return output
//	}
//	
//	
//	/**
//	 * Given an action return if it is a multicast
//	 */
//	def boolean isMulticast(Action action){
//		action.eAllOfType(MultiCast).size > 0 
//	}
//	
//	/**
//	 * Given an action return if it is a spont
//	 */
//	def boolean isSpont(Action action){
//		action.eAllOfType(SpontaneousAction).size > 0
//	}
//
//	/**
//	 * Given a an attribute name, return the value of the attribute in the environment 
//	 */
//	def String getValueEnv(Model model, String attributeName){
//		var String output = ""
//		var attributes = model.environmentAttributes
//		
//		for(attribute : attributes){
//			if(attribute.name.label.equals(attributeName))
//				output = attribute.label
//		}
//		
//		return output
//	}
//	/**
//	 * Given a component name, and an attribute name, return the value of the attribute
//	 */
//	def String getValueEnv(Model model, String attributeName, String recordName){
//		var String output = ""
//		var attributes = model.environmentAttributes
//		
//		for(attribute : attributes){
//			if(attribute.name.label.equals(attributeName)){
//				var rds = attribute.eAllOfType(RecordDeclaration)
//					for(rd : rds){
//						if(rd.name.label.equals(recordName))
//							output = rd.assign.label
//					}
//				
//			}
//				
//				
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Given a component name, and an attribute name, return the value of the attribute
//	 */
//	def String getValue(Model model, String componentName, String attributeName){
//		var String output = ""
//		var component = componentName.getComponent(model)
//		var attributes = component.eAllOfType(VariableDeclaration)
//		
//		for(attribute : attributes){
//			if(attribute.name.label.equals(attributeName))
//				output = attribute.label
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Given a component name, and an attribute name, return the value of the attribute
//	 */
//	def String getValue(Model model, String componentName, String attributeName, String recordName){
//		var String output = ""
//		var component = componentName.getComponent(model)
//		var attributes = component.eAllOfType(VariableDeclaration)
//		
//		for(attribute : attributes){
//			if(attribute.name.label.equals(attributeName)){
//				var rds = attribute.eAllOfType(RecordDeclaration)
//					for(rd : rds){
//						if(rd.name.label.equals(recordName))
//							output = rd.assign.label
//					}
//				
//			}
//				
//				
//		}
//		
//		return output
//	}
//	
//	def String convertType(VariableDeclaration vd){
//		if(vd.type.toString.equals("enum")){
//			return "Integer"
//		}
//		if(vd.type.toString.equals("record")){
//			return "Integer"
//		}
//		if(vd.type.toString.equals("double")){
//			return "Double"
//		}
//		if(vd.type.toString.equals("integer")){
//			return "Integer"
//		}
//	}
//	
//	def String convertType(VariableReference vd){
//		if(vd.type.toString.equals("enum")){
//			return "Integer"
//		}
//		if(vd.type.toString.equals("record")){
//			return "Integer"
//		}
//		if(vd.type.toString.equals("double")){
//			return "Double"
//		}
//		if(vd.type.toString.equals("integer")){
//			return "Integer"
//		}
//	}
//	
//	def String convertPrimitiveType(VariableDeclaration vd){
//		if(vd.type.toString.equals("enum")){
//			return "int"
//		}
//		if(vd.type.toString.equals("record")){
//			return "int"
//		}
//		if(vd.type.toString.equals("double")){
//			return "double"
//		}
//		if(vd.type.toString.equals("integer")){
//			return "int"
//		}
//	}
//	
//	def String convertPrimitiveType(VariableReference vd){
//		if(vd.type.toString.equals("enum")){
//			return "int"
//		}
//		if(vd.type.toString.equals("record")){
//			return "int"
//		}
//		if(vd.type.toString.equals("double")){
//			return "double"
//		}
//		if(vd.type.toString.equals("integer")){
//			return "int"
//		}
//	}
//	
//	/**
//	 * Given a variable reference, find variabledeclaration from COMPONENT!
//	 */
//	def VariableDeclaration getVariableDeclaration(VariableReference vrr){
//		var components = new ArrayList<Component>(vrr.getContainerOfType(Model).eAllOfType(Component))
//		switch(vrr){
//			VariableReferencePure		: components.getVariableDeclaration(vrr.name)
//			VariableReferenceMy			: components.getVariableDeclaration(vrr.name)
//			VariableReferenceThis		: components.getVariableDeclaration(vrr.name)
//			VariableReferenceReceiver	: vrr.variableDeclarationAny
//			VariableReferenceSender		: vrr.variableDeclarationAny
//			VariableReferenceGlobal		: vrr.variableDeclarationEnv
//			RecordReferencePure			: components.getVariableDeclaration(vrr.name)
//			RecordReferenceMy			: components.getVariableDeclaration(vrr.name)
//			RecordReferenceThis			: components.getVariableDeclaration(vrr.name)
//			RecordReferenceReceiver		: vrr.variableDeclarationAny
//			RecordReferenceSender		: vrr.variableDeclarationAny
//			RecordReferenceGlobal		: vrr.variableDeclarationEnv
//		}
//	}
//	
//	/**
//	 * Given a variable reference, find full variabledeclaration from anywhere
//	 */
//	def VariableDeclaration getVariableDeclarationAny(VariableReference vrr){
//		var vds = vrr.getContainerOfType(Model).eAllOfType(VariableDeclaration)
//		for(v :vds){
//			if(v.name.sameName(vrr.name))
//				if(v.getFullDeclaration != null ){
//					return v.getFullDeclaration
//				}
//	
//		}
//		return null
//	}
//	
//	def VariableDeclaration getFullDeclaration(VariableDeclaration vd){
//		switch(vd){
//			VariableDeclarationEnum			:	if(vd.eAllOfType(VariableReference).size == 0){return vd}
//			VariableDeclarationCarmaDouble	:	if(vd.eAllOfType(VariableReference).size == 0){return vd}
//			VariableDeclarationCarmaIntger	: 	if(vd.eAllOfType(VariableReference).size == 0){return vd}
//			VariableDeclarationRecord		:	return (vd as VariableDeclarationRecord).getFullDeclaration
//		}
//		return null
//	}
//	
//	def VariableDeclaration getFullDeclaration(VariableDeclaration vd){
//		switch(vd){
//			VariableDeclarationEnum			:	return vd
//			VariableDeclarationCarmaDouble	:	return vd
//			VariableDeclarationCarmaIntger	: 	return vd
//			VariableDeclarationRecord		:	return (vd as VariableDeclarationRecord).getFullDeclaration
//		}
//		return null
//	}
//	
//	def VariableDeclaration getFullDeclaration(VariableDeclarationRecord vdr){
//		if(vdr.eAllOfType(Records).size > 0 || vdr.recordDeclarations.size > 0) {
//			return (vdr as VariableDeclaration)
//		}
//		return null
//	}
//	
//	/**
//	 * Given a variable reference, find variabledeclaration from Environment!
//	 */
//	def VariableDeclaration getVariableDeclarationEnv(VariableReference vrr){
//		var vds = vrr.getContainerOfType(Model).environmentAttributes
//		for(vd : vds)
//			if(vd.name.sameName(vrr.name))
//				return vd
//	}
//	
//	/**
//	 * Given a variable reference, find variabledeclaration from COMPONENT!
//	 */
//	def VariableDeclaration getVariableDeclaration(Name name){
//		var components = new ArrayList<Component>(name.getContainerOfType(Model).eAllOfType(Component))
//		components.getVariableDeclaration(name)
//	}
//	
//	def VariableDeclaration getVariableDeclaration(ArrayList<Component> components, Name name){
//		var VariableDeclaration output = null
//		for(component : components){
//			for(variableDeclaration : component.eAllOfType(VariableDeclaration))
//				if(name.sameName(variableDeclaration.name))
//					output = variableDeclaration
//		}
//		return output
//	}
//	
//	def boolean isRecord(Name name){
//		var vd = getVariableDeclaration(name)
//		switch(vd){
//			VariableDeclarationRecord: true
//			default: false
//		}
//	}
//	
//	def boolean isEnum(Name name){
//		var vd = getVariableDeclaration(name)
//		switch(vd){
//			VariableDeclarationEnum: true
//			default: false
//		}
//	}
//	
//	/**
//	 * Given a VariableDeclaration return all VariableDeclarations with the same name
//	 */
//	def ArrayList<VariableDeclaration> getSameDeclarations(VariableDeclaration vd){
//		var ArrayList<VariableDeclaration> output = new ArrayList<VariableDeclaration>()
//		
//		for(vardec : vd.getContainerOfType(Model).eAllOfType(VariableDeclaration)){
//			if(vardec.name.sameName(vd.name))
//				output.add(vardec)
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Given a VariableType return all VariableType with the same name
//	 */
//	def ArrayList<VariableType> getSameDeclarations(VariableType vt){
//		var ArrayList<VariableType> output = new ArrayList<VariableType>()
//		
//		for(vardec : vt.getContainerOfType(Model).eAllOfType(VariableType)){
//			if(vardec.name.sameName(vt.name))
//				output.add(vardec)
//		}
//		
//		return output
//	}
//
//	/**
//	 * Given a VariableDeclaration return boolean if all VariableDeclarations in the model
//	 * are the same type
//	 */
//	def boolean sameType(VariableDeclaration vd){
//		var others = vd.sameDeclarations
//		var boolean output = true
//		
//		for(o : others){
//			output = output && vd.type.toString.equals(o.type.toString)
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Given a VariableDeclaration return boolean if all VariableDeclarations in the model
//	 * are the same type
//	 */
//	def boolean sameType(VariableType vt){
//		var others = vt.sameDeclarations
//		var boolean output = true
//		
//		for(o : others){
//			output = output && vt.type.toString.equals(o.type.toString)
//		}
//		
//		return output
//	}
//	
//	/**
//	 * Is this a BlockSystem?
//	 */
//	def boolean isBlockSystem(System system){
//		switch(system){
//			BlockSystem: true
//			LineSystem:	false
//		}
//	}
//	
//	/**
//	 * Given a CBND return ComponentBlockDefinition
//	 */
//	def ComponentBlockDefinition getComponent(CBND cbnd){
//		var ComponentBlockDefinition output = null
//		var name = cbnd.name
//		
//		for(component : cbnd.getContainerOfType(Model).eAllOfType(ComponentBlockDefinition))
//			if(component.name.sameName(name))
//				output = component
//		
//		return output
//	}
//	
//	/**
//	 * Given a CBND check argument count and type of matching ComponentBlockDefinition
//	 */
//	def boolean hasMatchingArguments(CBND cbnd){
//		var cbndArguments = cbnd.eAllOfType(NCA)
//		var componentArguments = cbnd.component.eAllOfType(ComponentArgument)
//		if(cbndArguments.size != componentArguments.size)
//			return false
//		else{
//			var output = true
//			var count = 0
//			for(;count < cbndArguments.size;count++){
//				output = output && cbndArguments.get(count).type.toString.equals(componentArguments.get(count).type.toString)
//			}
//			
//			return output
//		}	
//	}
//	
//	/**
//	 * Given a CBND check argument count and type of matching ComponentBlockDefinition
//	 */
//	def boolean hasMatchingArguments(CBND cbnd, ComponentBlockDefinition cbd){
//		var cbndArguments = cbnd.eAllOfType(NCA)
//		var componentArguments = cbd.eAllOfType(ComponentArgument)
//		if(cbndArguments.size != componentArguments.size){
//			return false
//		}else{
//			var output = true
//			var count = 0
//			for(;count < cbndArguments.size;count++){
//				output = output && cbndArguments.get(count).type.toString.equals(componentArguments.get(count).type.toString)
//			}
//			
//			return output
//		}	
//	}
//	
//	/**
//	 * Given a component return all declarations
//	 */
//	def HashMap<Component,ArrayList<CBND>> getComponentToCBNDs(ComponentBlockDefinition cbd){
//		var HashMap<Component,ArrayList<CBND>> output = new HashMap<Component,ArrayList<CBND>>()
//		
//		
//		if(cbd != null){
//			var cbnds = new ArrayList<CBND>()
//			for(cbnd : cbd.getContainerOfType(Model).eAllOfType(CBND)){
//				if(cbd.name.sameName(cbnd.name)){
//					if(cbnd.hasMatchingArguments(cbd)){
//						cbnds.add(cbnd)
//					}
//				}
//			}
//			output.put(cbd,cbnds)
//			
//		}
//		return output
//	}
//	
//	def HashMap<String,String> getNameValue(ArrayList<VariableName> vns){
//		
//		var HashMap<String,String> output = new HashMap<String,String>()
//		for(vn : vns){
//			output.putAll(vn.nameValueLabel)
//		}
//		return output
//		
//	}
//	
//	/**
//	 * Given a Component get MacroExpressionReference
//	 */
//	def ArrayList<MacroExpressionReference> getMacros(Component component){
//		
//		switch(component){
//			ComponentBlockDefinition: 	return (component as ComponentBlockDefinition).getMacrosBlock
//			default:					return (component as ComponentLineDefinition ).getMacrosLine
//		}
//		
//		
//	}
//	
//	def ArrayList<MacroExpressionReference> getMacrosBlock(ComponentBlockDefinition component){
//		var ArrayList<MacroExpressionReference> macros = new ArrayList<MacroExpressionReference>()
//		
//		var inits = component.eAllOfType(MacroExpressionReference)
//		var processes = component.getContainerOfType(Model).eAllOfType(Process)
//		
//		for(p : processes){
//			for(i : inits)
//				if(p.name.sameName(i.name))
//					macros.add(i)
//		}
//		
//		var cas = component.componentArguments.eAllOfType(ComponentArgument)
//		for(ca : cas){
//			if(ca.eAllOfType(ProcessName).size > 0)
//				for(i : inits){
//					if(i.name.sameName(ca.eAllOfType(ProcessName).get(0))){
//						var cToCBND = ca.componentToCBNDs
//						for(key : cToCBND.keySet){
//							for(cbnd : cToCBND.get(key)){
//								macros.addAll(cbnd.eAllOfType(NCA).get(ca.position).eAllOfType(MacroExpressionReference))
//							}
//						}
//					}
//				}
//		}
//		
//		return macros
//	}
//	
//	def ArrayList<MacroExpressionReference> getMacrosLine(ComponentLineDefinition component){
//		new ArrayList<MacroExpressionReference>(component.eAllOfType(MacroExpressionReference))
//	}
//	
//	/**
//	 * Given a VariableTypeRecord, return its ComponentBlockNewDeclarations
//	 */
//	def HashMap<Component,ArrayList<CBND>> getComponentToCBNDs(VariableTypeRecord vtr){
//		return vtr.getContainerOfType(ComponentArgument).getComponentToCBNDs
//	}
//	
//	/**
//	 * Given a VariableName, return its ComponentBlockNewDeclarations
//	 */
//	def HashMap<Component,ArrayList<CBND>> getCBNDs(VariableName vtr){
//		return vtr.getContainerOfType(ComponentArgument).getComponentToCBNDs
//	}
//	
//	/**
//	 * Given a ComponentArgument, return its ComponentBlockNewDeclarations
//	 */
//	def HashMap<Component,ArrayList<CBND>> getComponentToCBNDs(ComponentArgument ca){
//		ca.getContainerOfType(ComponentBlockDefinition).getComponentToCBNDs
//	}
//	
//	def int getPosition(ComponentArgument ca){
//		return ca.getContainerOfType(ComponentBlockDefinition).componentArguments.eAllOfType(ComponentArgument).indexOf(ca)
//	}
//	
//	def int getPosition(CBND cbnd, NCA nca){
//		return cbnd.eAllOfType(NCA).indexOf(nca)
//	}
//	
//	/**
//	 * Given a VariableTypeRecord, return position inside ComponentBlockDefinitionArguments or MethodDefinitionArguments
//	 */
//	def int getPosition(VariableTypeRecord vtr){
//		var position = -1
//		
//		if(vtr.getContainerOfType(ComponentBlockDefinitionArguments) != null){
//			var arguments = vtr.getContainerOfType(ComponentBlockDefinitionArguments).eAllOfType(ComponentArgument)
//			for(argument : arguments){
//				if(argument.eAllOfType(Name).get(0).sameName(vtr.name)){
//					position++
//					return position
//				} else {
//					position++
//				}
//			}
//		}
//		
//		if(vtr.getContainerOfType(MethodDefinitionArguments) != null){
//			var arguments = vtr.getContainerOfType(MethodDefinitionArguments).eAllOfType(MethodDefinitionArgument)
//			for(argument : arguments){
//				if(argument.eAllOfType(Name).get(0).sameName(vtr.name)){
//					position++
//					return position
//				} else {
//					position++
//				}
//			}
//		}
//		
//		return position
//	}
//	
//	/**
//	 * Given a VariableDeclarationRecord, return position inside ComponentBlockDefinitionArguments or MethodDefinitionArguments
//	 */
//	def int getPosition(VariableDeclarationRecord vdr){
//		var position = -1
//		
//		if(vdr.getContainerOfType(ComponentBlockDefinitionArguments) != null){
//			var arguments = vdr.getContainerOfType(ComponentBlockDefinitionArguments).eAllOfType(ComponentArgument)
//			for(argument : arguments){
//				if(argument.eAllOfType(Name).get(0).sameName(vdr.assign.ref)){
//					position++
//					return position
//				} else {
//					position++
//				}
//			}
//		}
//		
//		if(vdr.getContainerOfType(MethodDefinitionArguments) != null){
//			var arguments = vdr.getContainerOfType(MethodDefinitionArguments).eAllOfType(MethodDefinitionArgument)
//			for(argument : arguments){
//				if(argument.eAllOfType(Name).get(0).sameName(vdr.assign.ref)){
//					position++
//					return position
//				} else {
//					position++
//				}
//			}
//		}
//		
//		return position
//	}
//	
//	def ArrayList<RecordDeclaration> getRecordDeclarationsFromCBND(VariableName vn){
//		
//		//get position in the ComponentBlockDefinitionArguments
//		var position = vn.getPosition
//		//get ComponentBlockDeclaration
//		var cbnds = vn.getCBNDs
//		var output = new ArrayList<RecordDeclaration>()
//		for(cd : cbnds.keySet){
//			for(c : cbnds.get(cd)){
//				switch(c){
//					ComponentBlockNewDeclaration		: output.addAll((c as ComponentBlockNewDeclaration).componentInputArguments.inputArguments.get(position).eAllOfType(RecordDeclaration))
//					ComponentBlockNewDeclarationSpawn	: output.addAll((c as ComponentBlockNewDeclarationSpawn).componentInputArguments.inputArguments.get(position).recordDeclarationsFromNewComponentArgumentSpawn)
//				}
//			}
//		}
//		return output
//			
//	}
//	
//	def ArrayList<RecordDeclaration> getRecordDeclarationsFromNewComponentArgumentSpawn(NCA nca){
//		switch(nca){
//			NewComponentArgumentSpawnPrimitive	:	null
//			NewComponentArgumentSpawnDeclare	:	new ArrayList<RecordDeclaration>(nca.eAllOfType(RecordDeclaration))
//			NewComponentArgumentSpawnMacro		:	null
//			NewComponentArgumentSpawnMethod		:	null
//			NewComponentArgumentSpawnReference	:	new ArrayList<RecordDeclaration>((nca.value.variableDeclaration as VariableDeclarationRecord).recordDeclarations)
//		}
//		
//	}
//	
//	def int getPosition(VariableName vn){
//		var position = -1
//		if(vn.getContainerOfType(ComponentBlockDefinitionArguments) != null){
//			var arguments = vn.getContainerOfType(ComponentBlockDefinitionArguments).eAllOfType(ComponentArgument)
//			for(argument : arguments){
//				if(argument.eAllOfType(Name).get(0).sameName(vn)){
//					position++
//					return position
//				} else {
//					position++
//				}
//			}
//		}
//		
//		return position
//	}
//	
//	def ArrayList<VariableDeclarationRecord> getAll(VariableDeclarationRecord vdr){
//		var vdrs = vdr.getContainerOfType(Model).eAllOfType(VariableDeclarationRecord)
//		var ArrayList<VariableDeclarationRecord> output = new ArrayList<VariableDeclarationRecord>()
//		
//		for(v : vdrs)
//			if(v.name.sameName(vdr.name))
//				output.add(v)
//		
//		return output
//	}
//	
//	def ArrayList<Records> getAllRecords(VariableDeclarationRecord vdr){
//		var ArrayList<Records> records = new ArrayList<Records>()
//		
//		var vdrs = vdr.getAll
//		
//		for(v : vdrs)
//			records.addAll(v.eAllOfType(Records))
//		
//		return records
//	}
//	
//	def int getPosition(Records r){
//		var output = -1
//		var flat = r.flatten
//		
//		if(r.getContainerOfType(ComponentBlockNewDeclarationSpawn) != null){
//			for (nca : r.getContainerOfType(ComponentBlockNewDeclarationSpawn).eAllOfType(NCA)){
//				if(nca.flatten.equals(flat)){
//					output++
//					return output
//				} else {
//					output++
//				}
//			}
//		}
//		
//		if(r.getContainerOfType(ComponentBlockNewDeclaration) != null){
//			for (nca : r.getContainerOfType(ComponentBlockNewDeclaration).eAllOfType(NCA)){
//				if(nca.flatten.equals(flat)){
//					output++
//					return output
//				} else {
//					output++
//				}
//			}
//		}
//		return output
//	}
//	
//	def String satisfiesPrefix(VariableReference vr, String message){
//		switch(vr){
//			VariableReferencePure		: 	vr.prefixVariableReferencePure(message)
//			VariableReferenceMy			: 	vr.prefixComponent(message)
//			VariableReferenceThis		: 	vr.prefixComponent(message)
//			VariableReferenceReceiver	: 	vr.prefixInputComponent(message)  
//			VariableReferenceSender		:	vr.prefixOutputComponent(message)
//			VariableReferenceGlobal		:	vr.prefixGlobal(message)
//			RecordReferencePure			: 	vr.prefixVariableReferencePure(message)
//			RecordReferenceMy			: 	vr.prefixComponent(message)
//			RecordReferenceThis			: 	vr.prefixComponent(message)
//			RecordReferenceReceiver		: 	vr.prefixInputComponent(message)
//			RecordReferenceSender		:	vr.prefixOutputComponent(message)
//			RecordReferenceGlobal		:	vr.prefixGlobal(message)
//		}
//	}
//	
//	//component or global_store - depends on context
//	def String prefixVariableReferencePure(VariableReference vr, String message){
//		if(vr.getContainerOfType(Component) != null){
//			//if it is in a predicate, then it must reference the "other guy's" Store
//			if(vr.getContainerOfType(ActionGuard) != null){
//				var action = vr.getContainerOfType(Action)
//				if(action.isOutput){
//					if(vr.otherComponentHasVariableOutput)
//						return ""
//					else
//						return message + " in all Components performing opposite action."
//				} else {
//					if(vr.otherComponentHasVariableInput)
//						return ""
//					else
//						return message + " in Input arguments."
//				}
//				
//			}
//			if(vr.componentHasVariableAnywhere)
//				return ""
//			else
//				return message + " in Component."
//		} 
//		if(vr.getContainerOfType(Environment) != null){
//			if(vr.environmentHasVariable)
//				return ""
//			else
//				return message + " in Global Store."
//		}
//		if(vr.getContainerOfType(Component) == null && vr.getContainerOfType(Process) != null){
//			//if it is in a predicate, then it must reference the "other guy's" Store
//			if(vr.getContainerOfType(ActionGuard) != null){
//				var action = vr.getContainerOfType(Action)
//				if(action.isOutput){
//					if(vr.otherComponentHasVariableOutput)
//						return ""
//					else
//						return message + " in all Components performing opposite action."
//				} else {
//					if(vr.otherComponentHasVariableInput)
//						return ""
//					else
//						return message + " in Input arguments."
//				}
//				
//			}
//			if(vr.allComponentHaveVariable)
//				return ""
//			else
//				return message + " in Component."
//		}
//		if(vr.getContainerOfType(Measure) != null){
//			return ""
//		}
//		if(vr.getContainerOfType(MethodDefinition) != null){
//			return ""
//		}
//		if(vr.getContainerOfType(ComponentBlockForStatement) != null){
//			return ""
//		}
//		message + "."
//	}
//	
//	def String prefixComponent(VariableReference vr, String message){
//		if(vr.getContainerOfType(Component) != null){
//			if(vr.componentHasVariable)
//				return ""
//			else
//				return message + " in Component."
//		}
//		if(vr.getContainerOfType(Component) == null && vr.getContainerOfType(Process) != null){
//			if(vr.allComponentHaveVariable)
//				return ""
//			else
//				return message + " in Component."
//		}
//		message + "."
//	}
//	
//	//check component with input action for attribute 
//	def String prefixInputComponent(VariableReference vr, String message){
//		if(vr.getContainerOfType(Environment) != null){
//			if(vr.componentWithInputActionHasVariableEnv)
//				return ""
//			else
//				return message + " in all Components performing input action."
//		}
//		message + "."
//	}
//	
//	//check component with output action for attribute 
//	def String prefixOutputComponent(VariableReference vr, String message){
//		if(vr.getContainerOfType(Environment) != null){
//			if(vr.componentWithOutputActionHasVariableEnv)
//				return ""
//			else
//				return message + " in all Components performing output action."
//		}
//		message + "."
//	}
//	
//	//global only - can only be in the global store
//	def String prefixGlobal(VariableReference vr, String message){
//		if(vr.getContainerOfType(Environment) != null){
//			if(vr.environmentHasVariable)
//				return ""
//			else
//				return message + " in Global Store."
//		}
//		message + "."
//	}
//	
//	def boolean componentHasVariableAnywhere(VariableReference vr){
//		if(vr.getContainerOfType(StoreBlock) != null){
//			var test = false
//			for(vd : vr.getContainerOfType(Component).eAllOfType(VariableDeclaration))
//				test = test || vd.name.sameName(vr.name)
//			for(vd : vr.getContainerOfType(Component).eAllOfType(VariableType))
//				test = test || vd.name.sameName(vr.name)
//			return test
//		} else if(vr.getContainerOfType(Process) != null){
//			var test = false
//			for(vd : vr.getContainerOfType(Component).eAllOfType(VariableDeclaration))
//				test = test || vd.name.sameName(vr.name)
//			if(vr.getContainerOfType(Component).eAllOfType(InputActionArguments).size > 0)
//				for(vd : vr.getContainerOfType(Component).eAllOfType(InputActionArguments).get(0).inputArguments)
//					test = test || (vd as VariableName).sameName(vr.name)
//			return test
//		} else {
//			false
//		}
//	}
//	
//	def boolean allComponentHaveVariable(VariableReference vr){
//		
//		var cs = vr.getContainerOfType(Process).componentAndDeclarations.keySet
//		var test = true
//		
//		for(c : cs)
//			test = test && c.componentHasVariable(vr)
//		
//		return test
//	}
//	
//	def boolean componentHasVariable(Component c, VariableReference vr){
//		var test = false
//		for(vd : c.eAllOfType(VariableDeclaration))
//			test = test || vd.name.sameName(vr.name)
//		return test
//	}
//	
//	def boolean componentHasVariable(VariableReference vr){
//		if(vr.getContainerOfType(Component) != null){
//			var test = false
//			for(vd : vr.getContainerOfType(Component).eAllOfType(VariableDeclaration))
//				test = test || vd.name.sameName(vr.name)
//			return test
//		} else {
//			false
//		}
//	}
//	
//	def boolean environmentHasVariable(VariableReference vr){
//		if(vr.getContainerOfType(Environment) != null){
//			var test = false
//			for(vd : vr.getContainerOfType(Model).environmentAttributes)
//				test = test || vd.name.sameName(vr.name)
//			return test
//		} else {
//			false
//		}
//	}
//	
//	//assumes the input arguments checks that the sender is sending the correct arguments
//	def boolean otherComponentHasVariableInput(VariableReference vr){
//		var test = false
//		if(vr.getContainerOfType(ActionGuard) != null){
//			var inputArgs = vr.getContainerOfType(Action).eAllOfType(InputActionArguments).get(0).inputArguments
//			
//			for(i : inputArgs){
//				test = test || (i as VariableName).sameName(vr.name)
//			}
//		}
//		return test
//	}
//	
//	
//	def boolean otherComponentHasVariableOutput(VariableReference vr){
//		var test = false
//		var ArrayList<Action> actions = new ArrayList<Action>()
//		var cs = new ArrayList<Component>()
//		if(vr.getContainerOfType(Action) != null){
//			var action = vr.getContainerOfType(Action)
//			if(action.spont){
//				test = true
//			}
//			actions.addAll(action.getOpposite)
//			for(a : actions){
//				cs.addAll(a.getContainerOfType(Process).componentAndDeclarations.keySet)
//			}
//			if(cs.size > 0)
//				test = true
//			for(c : cs)
//				test = test && c.componentHasVariable(vr)
//		}
//		return test
//	}
//	
//	def ArrayList<Action> getOpposite(Action action){
//		var ArrayList<Action> output = new ArrayList<Action>()
//		if(action.eAllOfType(InputAction).size != 0){
//			var op = action.eAllOfType(InputAction).get(0).sender
//			for(o : op)
//				output.add(o.getContainerOfType(Action))
//		} 
//		else{
//			var op = action.eAllOfType(OutputAction).get(0).receiver
//			for(o : op)
//				output.add(o.getContainerOfType(Action))
//		}
//		return output
//	}
//	
//	def boolean componentWithInputActionHasVariableEnv(VariableReference vr){
//		if(vr.getContainerOfType(Environment) != null){
//			var actionStub = vr.getContainerOfType(EnvironmentOperation).eAllOfType(ActionStub).get(0)
//			var processes = actionStub.processes
//			var actions = new ArrayList<Action>()
//			var cs = new ArrayList<Component>()
//			for(p : processes){
//				for(key : p.componentAndDeclarations.keySet)
//					for(a : key.actionsFromComponent){
//						if(!a.isOutput){
//							if(a.name.sameName(actionStub.name))
//								actions.add(a)
//						}
//					}
//			}
//			for(a : actions){
//				cs.addAll(a.getContainerOfType(Process).componentAndDeclarations.keySet)
//			}			
//			var test = true
//			for(c : cs)
//				test = test && c.componentHasVariable(vr)
//			return test
//		} else {
//			false
//		}
//	}
//	
//	def boolean componentWithOutputActionHasVariableEnv(VariableReference vr){
//		if(vr.getContainerOfType(Environment) != null){
//			var actionStub = vr.getContainerOfType(EnvironmentOperation).eAllOfType(ActionStub).get(0)
//			var processes = actionStub.processes
//			var actions = new ArrayList<Action>()
//			var cs = new ArrayList<Component>()
//			for(p : processes)
//				for(key : p.componentAndDeclarations.keySet)
//					for(a : key.actionsFromComponent){
//						if(a.isOutput){
//							if(a.name.sameName(actionStub.name))
//								actions.add(a)
//						}
//					}
//			for(a : actions){
//				cs.addAll(a.getContainerOfType(Process).componentAndDeclarations.keySet)
//			}		
//			var test = true
//			for(c : cs)
//				test = test && c.componentHasVariable(vr)
//			return test
//		} else {
//			false
//		}
//	}
//	
//	def HashSet<VariableReference> getGlobals(BooleanExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferencePure)) 
//		vrs.addAll(br.eAllOfType(RecordReferencePure))
//		vrs.addAll(br.eAllOfType(VariableReferenceGlobal)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceGlobal))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getSenders(BooleanExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceSender)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceSender))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getReceivers(BooleanExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceReceiver)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceReceiver))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getOutputTheirStores(BooleanExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferencePure)) 
//		vrs.addAll(br.eAllOfType(RecordReferencePure))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getOutputMyStores(BooleanExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceMy))
//		vrs.addAll(br.eAllOfType(VariableReferenceThis))		
//		vrs.addAll(br.eAllOfType(RecordReferenceMy))			
//		vrs.addAll(br.eAllOfType(RecordReferenceThis))				
//		
//		return vrs
//	}
//
//
//	def HashSet<VariableReference> getGlobals(EnvironmentUpdateExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferencePure)) 
//		vrs.addAll(br.eAllOfType(RecordReferencePure ))
//		vrs.addAll(br.eAllOfType(VariableReferenceGlobal)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceGlobal))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getSenders(EnvironmentUpdateExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceSender)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceSender))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getReceivers(EnvironmentUpdateExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceReceiver)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceReceiver))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getGlobals(EnvironmentExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferencePure)) 
//		vrs.addAll(br.eAllOfType(RecordReferencePure ))
//		vrs.addAll(br.eAllOfType(VariableReferenceGlobal)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceGlobal))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getSenders(EnvironmentExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceSender)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceSender))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getReceivers(EnvironmentExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceReceiver)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceReceiver))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getSenders(UpdateExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceSender)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceSender))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getReceivers(UpdateExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferenceReceiver)) 
//		vrs.addAll(br.eAllOfType(RecordReferenceReceiver))
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getStores(BooleanExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferencePure)) 
//		vrs.addAll(br.eAllOfType(RecordReferencePure))
//		vrs.addAll(br.eAllOfType(VariableReferenceMy))
//		vrs.addAll(br.eAllOfType(VariableReferenceThis))		
//		vrs.addAll(br.eAllOfType(RecordReferenceMy))			
//		vrs.addAll(br.eAllOfType(RecordReferenceThis))				
//		
//		return vrs
//	}
//	
//	def HashSet<VariableReference> getStores(UpdateExpressions br){
//		var HashSet<VariableReference> vrs 	= new HashSet<VariableReference>()
//		
//		vrs.addAll(br.eAllOfType(VariableReferencePure)) 
//		vrs.addAll(br.eAllOfType(RecordReferencePure))
//		vrs.addAll(br.eAllOfType(VariableReferenceMy))
//		vrs.addAll(br.eAllOfType(VariableReferenceThis))		
//		vrs.addAll(br.eAllOfType(RecordReferenceMy))			
//		vrs.addAll(br.eAllOfType(RecordReferenceThis))				
//		
//		return vrs
//	}
//	
//	def ArrayList<RecordDeclaration> getRecordDeclarations(VariableDeclarationRecord vdr){
//		var ArrayList<RecordDeclaration> rds = new ArrayList<RecordDeclaration>()
//		if(vdr.assign.ref != null){
//			rds.addAll(vdr.assign.ref.getRecordDeclarationsFromCBND)
//		} else {
//			rds.addAll(vdr.eAllOfType(RecordDeclaration))
//		}
//		return rds
//	}
//	
//	def boolean hasAccess(Component component, MacroExpressions macro){
//		var mers = macro.eAllOfType(MacroExpressionReference)
//		var HashSet<String> references = new HashSet<String>()
//		
//		for(mer : mers)
//			references.add("state_"+mer.name.label)
//		
//		var HashSet<String> states = new HashSet<String>()
//		component.tree.getStates(states)
//		
//		return states.containsAll(references)
//	}
//	
}