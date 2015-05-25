package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import static extension org.eclipse.xtext.EcoreUtil2.*
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclaration
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.NCA
import java.util.ArrayList
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.carma.LineSystem
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.ComponentArgument
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.RecordDeclaration

class GenerateSystems {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	
	def String declareComponents(System system){
		switch(system){
			BlockSystem: 	{
				var declaredComponents = system.getContainerOfType(Model).eAllOfType(ComponentBlockNewDeclaration)
				var output = ""
				for(dc : declaredComponents){
					if(dc.getContainerOfType(ComponentBlockForStatement) != null){
						output = output + dc.getContainerOfType(ComponentBlockForStatement).forBlock
					}else{
						if((dc.eAllOfType(Range).size > 0)){
							output = output + rangeDeclaration(dc)
						}else{
							output = output + singleDeclaration(dc)
					}
				}
		}
		return output}
			//TODO
			LineSystem:		{"//TODO"}
		}
		
	}
	
	def String forBlock(ComponentBlockForStatement cbfs){
		'''
		for(«cbfs.variable.convertToJava»;«cbfs.expression.label»;«cbfs.afterThought.nameValue»){
			«(cbfs.componentBlockForBlock.component as ComponentBlockNewDeclaration).singleDeclaration»
		};
		'''
		
	}
	
	def String rangeDeclaration(ComponentBlockNewDeclaration dc){
		var output = ""
		var arguments = dc.eAllOfType(NCA)
		var ArrayList<NCA> temp = new ArrayList<NCA>()
		
		//We don't want to pick up the Macro
		for(argument : arguments){
			if(argument.type.toString.equals("component"))
				temp.add(argument)
		}
		
		var ArrayList<ArrayList<String>> array1 = new ArrayList<ArrayList<String>>()
		
		for(argument : temp){
			array1.add(argument.strip)
		}
		var array2 = new ArrayList<ArrayList<String>>()
		forBlock(array1,array2)
		for(list : array2){
			output = output + singleDeclaration(dc, list)
		}
		return output
	}
	
	def String singleDeclaration(ComponentBlockNewDeclaration dc, ArrayList<String> args){
		'''
		addComponent(get«dc.label.toFirstUpper»(«args.stripArguments»));
		'''
	}
	
	def String stripArguments(ArrayList<String> args){
		
		var output = ""
		
		//now create the string
		if(args.size > 0){
			output = args.get(0)
			for(var i = 1; i < args.size; i++)
				output = output + "," + args.get(i)
		}
		
		return output
	}
	
	def void forBlock(ArrayList<ArrayList<String>> array, ArrayList<ArrayList<String>> output){
		if(array.size > 1){
			var head = array.remove(0)
			var exit = new ArrayList<ArrayList<String>>()
			forBlock(array,output)
			for(var i = 0; i < output.size; i++){
				for(item : head){
					var inter = new ArrayList<String>()
					inter.add(item)
					inter.addAll(output.get(i))
					exit.add(inter)
				}
			}
			output.clear
			output.addAll(exit)
		} else {
			var head = array.remove(0)
			for(item : head){
				var tail = new ArrayList<String>()
				tail.add(item)
				output.add(tail)
			}
		}
	}
	
	
	
	def ArrayList<String> strip(NCA nca){
		var temp = new ArrayList<String>()
		
		if(nca.eAllOfType(Range).size > 0)
			temp.addAll(nca.eAllOfType(Range).get(0).range)
		else
			temp.add(nca.label)

		return temp
	}
	
	def ArrayList<String> getRange(Range r){
		var ArrayList<String> output = new ArrayList<String>()
		for(var i = r.min; i <= r.max; i++){
			output.add(""+i)	
		}
		return output
	}
	
	def String singleDeclaration(ComponentBlockNewDeclaration dc){
		'''
		addComponent(get«dc.label.toFirstUpper»(«dc.stripArguments»));
		'''
	}
	
	def String stripArguments(ComponentBlockNewDeclaration dc){
		
		var arguments = dc.eAllOfType(NCA)
		var output = ""
		var ArrayList<NCA> temp = new ArrayList<NCA>()
		
		//We don't want to pick up the Macro
		for(argument : arguments){
			if(argument.type.toString.equals("component"))
				temp.add(argument)
		}
		
		//now create the string
		if(temp.size > 0){
			output = arguments.get(0).getLabel
			for(var i = 1; i < temp.size; i++)
				output = output + "," + temp.get(i).getLabel
		}
		
		return output
	}
	
	def String getArgs(Component component){
		if(component.getContainerOfType(ComponentBlockDefinition) != null){
			
			var arguments = component.eAllOfType(ComponentArgument)
			var output = ""
			var ArrayList<ComponentArgument> temp = new ArrayList<ComponentArgument>()
			
			//We don't want to pick up the Macro
			for(argument : arguments){
				if(argument.type.toString.equals("component"))
					temp.add(argument)
			}
			
			//now create the string
			if(temp.size > 0){
				output = arguments.get(0).convertToJava
				for(var i = 1; i < temp.size; i++)
					output = output + "," + temp.get(i).convertToJava
			}
			
			return output
			
		} else {
			//TODO
			return "//TODO"
		}
	}
	
	def String defineComponents(System system){
		var components = new ArrayList<Component>(system.getContainerOfType(Model).eAllOfType(Component))
		var cat = system.getContainerOfType(Model).getComponentAttributeType
		'''
		«FOR component : components»
			private CarmaComponent get«component.label.toFirstUpper»(«component.getArgs») {
				CarmaComponent c4rm4 = new CarmaComponent();
				«var vds = cat.get(component)»
				«FOR vd : vds»
					«switch(vd){
						VariableDeclarationEnum: '''c4rm4.set( «system.getContainerOfType(Model).label»Definition.«vd.name.label.toUpperCase»_ATTRIBUTE, «system.getContainerOfType(Model).getValue(component.label,vd.name.label)»);'''
						VariableDeclarationRecord: {
							var rds = vd.eAllOfType(RecordDeclaration)
							'''
							«FOR rd : rds»
							c4rm4.set( «system.getContainerOfType(Model).label»Definition.«vd.name.label.toUpperCase»_«rd.name.label.toUpperCase»_ATTRIBUTE, «system.getContainerOfType(Model).getValue(component.label,vd.name.label,rd.name.label)»);
							«ENDFOR»
							'''
						}
					}»
				«ENDFOR»
				«FOR mer : component.getMacros»
				c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
				«system.getContainerOfType(Model).label»Definition.«component.label»Process,
				«system.getContainerOfType(Model).label»Definition.«component.label»Process.getState("state_«mer.name.label»" ) );
				«ENDFOR»
				return c4rm4;
			}
			«ENDFOR»
		'''
	}
	
	def String compileSystem(System system, String packageName, String className, String modelName){
		'''
		«packageName»;
		
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.carma.*;
		import org.cmg.ml.sam.sim.SimulationEnvironment;
		public class «className» extends CarmaSystem {
			
			//constructor
			public «className»(){
				«system.declareComponents»
			}
			
			«system.defineComponents»
			
			/*ENVIRONMENT PROBABILITY*/
			@Override
			public double broadcastProbability(CarmaStore sender, CarmaStore receiver,
					int action) {
				// TODO Auto-generated method stub
				return 0;
			}
		
			@Override
			public double unicastProbability(CarmaStore sender, CarmaStore receiver,
					int action) {
				// TODO Auto-generated method stub
				return 0;
			}
			
			/*ENVIRONMENT RATE*/
			@Override
			public double broadcastRate(CarmaStore sender, int action) {
				/*needs action names, and then environment rate values*/
				«IF system.eAllOfType(Rate).size > 0»
					«var actionStubs = system.eAllOfType(Rate).get(0).eAllOfType(ActionStub)»
					«FOR actionStub : actionStubs»
					if (action == «system.getContainerOfType(Model).label»Definition.«actionStub.name.name.toUpperCase») {
						return «actionStub.getContainerOfType(Rate).expression.label»;
					}
					«ENDFOR»
				«ENDIF»
				
				return 0;
			}
		
			@Override
			public double unicastRate(CarmaStore sender, int action) {
				// TODO Auto-generated method stub
				return 0;
			}
			
			/*ENVIRONMENT UPDATE*/
			@Override
			public void broadcastUpdate(RandomGenerator random, CarmaStore sender,
					int action) {
				// TODO Auto-generated method stub
				
			}
		
			@Override
			public void unicastUpdate(RandomGenerator random, CarmaStore sender,
					int action) {
				// TODO Auto-generated method stub
				
			}
			
			/*MAIN*/
			public static void main( String[] argv ) {
				SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
					new «modelName»Factory()
				);
		
				system.simulate(100,50);
			}
			
		}
		'''
	}
	
}