package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.ComponentArgument
import eu.quanticol.carma.core.carma.ComponentBlockDefinition
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclaration
import eu.quanticol.carma.core.carma.EnvironmentGuard
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.EnvironmentUpdateAssignment
import eu.quanticol.carma.core.carma.LineSystem
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.PrimitiveType
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList

import static extension org.eclipse.xtext.EcoreUtil2.*
import java.util.HashSet
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpression
import eu.quanticol.carma.core.carma.EnvironmentUpdateExpressions
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.carma.EnvironmentMeasure
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.MeasureVariableDeclarations
import eu.quanticol.carma.core.carma.BlockSpawn
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.LineSpawn
import eu.quanticol.carma.core.carma.EnvironmentMacroExpressions
import java.util.HashMap
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnReference
import eu.quanticol.carma.core.carma.MethodExpressions
import eu.quanticol.carma.core.carma.Records

class GenerateSystems {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	@Inject extension GeneratorUtils
	@Inject extension GenerateSystemEnvironmentUpdate
	@Inject extension GenerateSystemRate
	@Inject extension GenerateSystemProbability
	@Inject extension GenerateDefinitions
	
	def String compileSystem(System system, String packageName){
		'''
		«packageName»;
		
		import org.apache.commons.math3.random.RandomGenerator;
		import org.cmg.ml.sam.sim.SimulationEnvironment;
		import eu.quanticol.carma.simulator.*;
		import org.cmg.ml.sam.sim.sampling.Measure;
		import org.cmg.ml.sam.sim.sampling.SamplingCollection;
		import org.cmg.ml.sam.sim.sampling.StatisticSampling;
		public class «system.label» extends CarmaSystem {
			
			//constructor
			public «system.label»(){
				«system.declareComponents»
				«system.setGlobalStore»
			}
			
			//define components
			«system.defineComponents»
			
			//predicates
			«system.defineEnvironmentProbPredicates»
			
			«system.defineEnvironmentRatesPredicates»
			
			«system.defineEnvironmentUpdatesPredicates»
			
			//evol rules
			«system.defineRates»
			
			«system.defineProbabilities»
			
			«system.defineEnvironmentUpdates»
			
			//measures
			«system.defineMeasures»
			
			//main
			«system.defineMain»
			
		}
		'''
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
							if(rds.size > 0){
								'''
								«FOR rd : rds»
								c4rm4.set( «system.getContainerOfType(Model).label»Definition.«vd.name.label.toUpperCase»_«rd.name.label.toUpperCase»_ATTRIBUTE, «system.getContainerOfType(Model).getValue(component.label,vd.name.label,rd.name.label)»);
								«ENDFOR»
								'''
							} else if (vd.recordDeclarations.size > 0) {
								rds = vd.recordDeclarations
								'''
								«FOR rd : rds»
								c4rm4.set( «system.getContainerOfType(Model).label»Definition.«vd.name.label.toUpperCase»_«rd.name.label.toUpperCase»_ATTRIBUTE, «rd.name.label»);
								«ENDFOR»
								'''
							}
								
						}
					}»
				«ENDFOR»
				«FOR mer : component.getMacros»
				c4rm4.addAgent( new CarmaSequentialProcess(c4rm4, 
				«system.getContainerOfType(Model).label»Definition.«component.label»Process,
				«system.getContainerOfType(Model).label»Definition.«component.label»Process.getState("state_«mer.name.label»" )));
				«ENDFOR»
				return c4rm4;
			}
			«ENDFOR»
		'''
	}
	
	def String setGlobalStore(System system){
		var envs = system.getContainerOfType(Model).environmentAttributes
		'''
		«FOR vd : envs»
		«switch(vd){
			VariableDeclarationEnum:	'''global_store.set(«system.getContainerOfType(Model).label»Definition.«vd.name.label.toUpperCase»_ATTRIBUTE,«system.getContainerOfType(Model).getValueEnv(vd.name.label)»);'''
			VariableDeclarationRecord:{
							var rds = vd.eAllOfType(RecordDeclaration)
							if(rds.size > 0){
								'''
								«FOR rd : rds»
								global_store.set( «system.getContainerOfType(Model).label»Definition.«vd.name.label.toUpperCase»_«rd.name.label.toUpperCase»_ATTRIBUTE, «system.getContainerOfType(Model).getValueEnv(vd.name.label,rd.name.label)»);
								«ENDFOR»
								'''
							} 
								
						}
		}»
		«ENDFOR»
		'''
			
	}
	
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
		var ArrayList<PrimitiveType> temp = new ArrayList<PrimitiveType>()
		
		//We don't want to pick up the Macro
		for(argument : arguments){
			if(argument.type.toString.equals("component"))
				for(pt : argument.eAllOfType(PrimitiveType))
					temp.add(pt)
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
	
	def ArrayList<String> strip(PrimitiveType pt){
		var temp = new ArrayList<String>()
		
		if(pt.eAllOfType(Range).size > 0)
			for(r : pt.eAllOfType(Range))
				temp.addAll(r.range)
		else
			temp.add(pt.label)


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
			output = arguments.get(0).getLabelForArgs
			for(var i = 1; i < temp.size; i++)
				output = output + "," + temp.get(i).getLabelForArgs
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
			
			SamplingCollection<CarmaSystem> sc = new SamplingCollection<CarmaSystem>();
			
			«system.setupMeasures»
			
			system.setSampling(sc);
			system.simulate(100,50);
			for(int i = 0; i < sc.size(); i++){
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
	
	def String defineMeasures(System system){
		var measures = new ArrayList<EnvironmentMeasure>(system.eAllOfType(EnvironmentMeasure))
		'''
		«FOR m : measures»
		«m.defineGetBooleanExpressionStateMeasure()»
		«m.defineGetBooleanExpressionPredicateMeasure()»
		//getMethod
		«m.defineGetMeasureMethod()»
		«ENDFOR»
		'''
	}
	
	def String defineGetBooleanExpressionStateMeasure(EnvironmentMeasure measure){
		
		var ems = (measure.componentReference as EnvironmentMacroExpressions)
		
		
		'''
		public static CarmaProcessPredicate get«measure.label»_State_Predicate(){
			return new CarmaProcessPredicate() {
				
				@Override
				public boolean eval(CarmaProcess p) {
					return «ems.booleanExpressionFromEMS»
				}
			};
		}
		'''
	}
	
	def String defineGetBooleanExpressionPredicateMeasure(EnvironmentMeasure measure){
		var exp = measure.booleanExpression
		'''
		protected static CarmaPredicate getPredicate«measure.label»() {
			return new CarmaPredicate() {
				@Override
				public boolean satisfy(CarmaStore store) {
					boolean hasAttributes = true;
					«exp.getAllVariablesMeasure»
					if(hasAttributes)
						return «exp.convertToJava»;
					else
						return false;
				}
			};
		}
		
		
		public static ComponentPredicate get«measure.label»_BooleanExpression_Predicate(){
			return new ComponentPredicate() {
				
				@Override
				public boolean eval(CarmaComponent c){
					return getPredicate«measure.label»().satisfy(c.getStore()) && (c.isRunning(get«measure.label»_State_Predicate()));
				}
			};
		}
		'''
	}
	
	def String defineGetMeasureMethod(EnvironmentMeasure measure){
		
		'''
		public static Measure<CarmaSystem> get«measure.label»(){
			
			return new Measure<CarmaSystem>(){
			
				ComponentPredicate predicate = get«measure.label»_BooleanExpression_Predicate();
			
				@Override
				public double measure(CarmaSystem t){
					//TODO
				
					return t.measure(predicate);
			
				};
			
				@Override
				public String getName() {
					return "«measure.label»";
				}
			};
		}
		'''
	}
	
	def String getBooleanExpressionFromEMS(EnvironmentMacroExpressions ems){
		var componentState = new HashMap<String,ArrayList<String>>()
		ems.emsExpression(componentState)
		var output = ""
		if(componentState.keySet.size > 0){
			output = output + '''
			( 
			(((CarmaSequentialProcess) p).automaton() ==  «ems.getContainerOfType(Model).label»Definition.«componentState.keySet.get(0)») && (
			'''
			for(state : componentState.get(componentState.keySet.get(0))){
				output = output + '''
				(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
				'''
			}
			output = output + '''
			(((CarmaSequentialProcess) p).getState() !=  null))
			)
			'''
			for(var int i = 1; i < componentState.keySet.size; i++){
				output = output + '''
				||( 
				(((CarmaSequentialProcess) p).automaton() ==  «ems.getContainerOfType(Model).label»Definition.«componentState.keySet.get(i)») && (
				'''
				for(state : componentState.get(componentState.keySet.get(i))){
					output = output + '''
					(((CarmaSequentialProcess) p).automaton().getState("«state»") != null ) ||
					'''
				}
				
				output = output + '''
				(((CarmaSequentialProcess) p).getState() !=  null))
				)
				'''
			}
		}
		return output + ";"
	}
	
}