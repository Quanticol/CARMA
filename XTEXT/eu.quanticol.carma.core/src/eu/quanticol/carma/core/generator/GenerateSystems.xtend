package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.BlockSpawn
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.EnvironmentMeasure
import eu.quanticol.carma.core.carma.LineSpawn
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnReference
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.generator.carmaVariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.components.ComponentDefine
import eu.quanticol.carma.core.generator.components.ComponentManager
import eu.quanticol.carma.core.generator.components.ComponentNew
import eu.quanticol.carma.core.generator.components.DeclareGlobalStore
import eu.quanticol.carma.core.generator.main.Main
import eu.quanticol.carma.core.generator.measures.MeasureManager
import eu.quanticol.carma.core.generator.predicates.Predicates
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*

class GenerateSystems {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension GeneratorUtils
	@Inject extension GenerateSystemEnvironmentUpdate
	@Inject extension GenerateSystemRate
	@Inject extension GenerateSystemProbability
	@Inject extension Predicates
	@Inject extension Main
	@Inject extension ComponentNew
	@Inject extension DeclareGlobalStore
	@Inject extension ComponentDefine
	
	def String compileSystem(System system, 
		String packageName,
		CarmaVariableManager variableManager, 
		ActionManager actionManager,
		ComponentManager componentManager,
		MeasureManager measureManager
		){
		var modelName = system.getContainerOfType(Model).label
		var systemName = system.label
		'''
		«packageName»;
		
		import java.util.ArrayList;
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.sim.SimulationEnvironment;
		import eu.quanticol.carma.simulator.*;
		import org.cmg.ml.sam.sim.sampling.Measure;
		import org.cmg.ml.sam.sim.sampling.SamplingCollection;
		import org.cmg.ml.sam.sim.sampling.StatisticSampling;
		public class «systemName» extends CarmaSystem {
			
			//constructor
			public «systemName»(){
				«componentManager.setupComponents»
				«variableManager.setGlobalStore(system)»
			}
			
			//define components
			«componentManager.defineComponents»
			
			//predicates
			«system.defineEnvironmentProbPredicates(variableManager)»
			
			«system.defineEnvironmentRatePredicates(variableManager)»
			
			«system.defineEnvironmentUpdatePredicates(variableManager)»
			
			//evol rules
			«system.defineRates»
			
			«system.defineProbabilities»
			
			«system.defineEnvironmentUpdates»
			
			//main
			«modelName.getMain(measureManager)»
			
		}
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
	

	
	def ArrayList<String> getRange(Range r){
		var ArrayList<String> output = new ArrayList<String>()
		for(var i = r.min; i <= r.max; i++){
			output.add(""+i)	
		}
		return output
	}
	
	def String setupMeasures(System system){
		if(system.getContainerOfType(Model).eAllOfType(MeasureBlock).size > 0){
			var measureBlock = system.getContainerOfType(Model).eAllOfType(MeasureBlock).get(0)
			var measures = measureBlock.eAllOfType(Measure)
			'''
			«FOR m : measures»
			«system.measureArgs(m)»
			«ENDFOR»
			'''
		} else {
			''''''
		}
	}
	
	def String measureArgs(System s, Measure m){
		var output = ""
		var arguments = m.parameters.eAllOfType(VariableDeclaration)
		
		var ArrayList<ArrayList<String>> array1 = new ArrayList<ArrayList<String>>()
		
		for(argument : arguments){
			array1.add(argument.strip)
		}
		
		var array2 = new ArrayList<ArrayList<String>>()
		forBlock(array1,array2)
		for(list : array2){
			output = output + singleMeasureDeclaration(s, m, list)
		}
		return output
	}
	
	def ArrayList<String> strip(VariableDeclaration pt){
		var temp = new ArrayList<String>()
		
		if(pt.eAllOfType(Range).size > 0)
			for(r : pt.eAllOfType(Range))
				temp.addAll(r.range)
		else
			temp.add(pt.label)


		return temp
	}
	
	def String singleMeasureDeclaration(System s, Measure m, ArrayList<String> args){
		var measureName = m.name.getLabel.toFirstUpper
		var stateName = (m.measure as EnvironmentMeasure).componentReference.getLabel.toFirstUpper
		'''
		sc.addSamplingFunction(new StatisticSampling<CarmaSystem>(deadline+1, 1.0, «s.getContainerOfType(Model).label»Definition.getMeasure«measureName»_«stateName»(«args.stripArguments»)));
		inputs.add("«args.stripArguments»");
		'''
	}
	
	def String defineMain(System system){
		'''
		/*MAIN*/
		public static void main( String[] argv ) {
			SimulationEnvironment<CarmaSystem> system = new SimulationEnvironment<CarmaSystem>(
				new «system.getContainerOfType(Model).label»Factory()
			);
		
			int deadline = 50; 
			ArrayList<String> inputs = new ArrayList<String>();
			SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
			
			«system.setupMeasures»
			
			system.setSampling(sc);
			system.simulate(100,50);
			for(int i = 0; i < sc.size(); i++){
				((StatisticSampling<CarmaSystem>) sc.get(i)).printName(System.out); if(inputs.size() > i){System.out.println(" with '" + inputs.get(i)+"' as arguments.");}
				((StatisticSampling<CarmaSystem>) sc.get(i)).printTimeSeries(System.out);
			}
		}'''
	}
	
	
	def String blockSpawn(BlockSpawn bs){
		var declaredComponents = bs.eAllOfType(ComponentBlockNewDeclarationSpawn)
		var String output = ""
		for(dc : declaredComponents){
			if((dc.eAllOfType(Range).size > 0)){
				output = output + rangeDeclaration(dc)
			}else{
				output = output + singleDeclaration(dc)
			}
		}
		return output
	}
	
	def String rangeDeclaration(ComponentBlockNewDeclarationSpawn dc){
		var output = '''
		«dc.getAllVariablesComponentBlockNewDeclarationSpawn»
		if(hasAttributes){
			
		'''
		var arguments = dc.eAllOfType(NCA)
		var ArrayList<NCA> temp = new ArrayList<NCA>()
		
		//We don't want to pick up the Macro
		for(argument : arguments){
			if(argument.type.toString.equals("component"))
				for(pt : argument.eAllOfType(NCA))
					temp.add(pt)
		}
		
		var ArrayList<ArrayList<String>> array1 = new ArrayList<ArrayList<String>>()
		
		for(argument : temp){
			switch(argument){
				NewComponentArgumentSpawnPrimitive 	: {
				if(argument.eAllOfType(Range).size > 0){
					for(r : argument.eAllOfType(Range)){
						array1.add(r.range)
					}
				}else{
					array1.addAll(argument.allVariablesNCA)
				}}
				NewComponentArgumentSpawnMethod		: array1.addAll(argument.allVariablesNCA)
				NewComponentArgumentSpawnDeclare	: {
					if(argument.eAllOfType(Range).size > 0){
						for(rec : argument.eAllOfType(RecordDeclaration)){
							if(rec.eAllOfType(Range).size > 0){
								for(r : rec.eAllOfType(Range)){
									array1.add(r.range)
								}
							} else {
								var temp2 = new ArrayList<String>()
								temp2.add(rec.getLabelForArgs)
								array1.add(temp2)
							}
						
						}
					}else{
						array1.addAll(argument.allVariablesNCA)
					}
				}
				NewComponentArgumentSpawnReference	: array1.addAll(argument.allVariablesNCA)
			}
		}
		var array2 = new ArrayList<ArrayList<String>>()
		forBlock(array1,array2)
		for(list : array2){
			output = output + singleDeclaration(dc, list)
		}
		
		output = output + "}"
		
		return output
	}
	
	def String singleDeclaration(ComponentBlockNewDeclarationSpawn dc, ArrayList<String> args){
		'''
		addComponent(get«dc.label.toFirstUpper»(«args.stripArguments»));
		'''
	}

	def String singleDeclaration(ComponentBlockNewDeclarationSpawn dc){
		'''
		«dc.getAllVariablesComponentBlockNewDeclarationSpawn»
		addComponent(get«dc.label.toFirstUpper»(«dc.getAllVariablesComponentBlockNewDeclarationSpawnArgs»));
		'''
	}

	
	def String lineSpawn(LineSpawn bs){
		//TODO
	}
	

	

	


	
}