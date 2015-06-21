package eu.quanticol.carma.core.generator

import com.google.inject.Inject
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.ActionGuard
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.BlockSpawn
import eu.quanticol.carma.core.carma.BlockSystem
import eu.quanticol.carma.core.carma.BooleanExpressions
import eu.quanticol.carma.core.carma.CarmaInteger
import eu.quanticol.carma.core.carma.Component
import eu.quanticol.carma.core.carma.ComponentArgument
import eu.quanticol.carma.core.carma.ComponentBlockDefinitionArgumentVariable
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclaration
import eu.quanticol.carma.core.carma.ComponentBlockNewDeclarationSpawn
import eu.quanticol.carma.core.carma.ComponentBlockStyleCollective
import eu.quanticol.carma.core.carma.EnumAssignment
import eu.quanticol.carma.core.carma.EnumAssignmentCarmaInteger
import eu.quanticol.carma.core.carma.EnumAssignmentMethodReference
import eu.quanticol.carma.core.carma.EnumAssignmentRange
import eu.quanticol.carma.core.carma.Environment
import eu.quanticol.carma.core.carma.EnvironmentMeasure
import eu.quanticol.carma.core.carma.EnvironmentOperation
import eu.quanticol.carma.core.carma.EnvironmentUpdate
import eu.quanticol.carma.core.carma.ForVariableDeclaration
import eu.quanticol.carma.core.carma.InputActionArguments
import eu.quanticol.carma.core.carma.Measure
import eu.quanticol.carma.core.carma.MeasureBlock
import eu.quanticol.carma.core.carma.MeasureVariableDeclarations
import eu.quanticol.carma.core.carma.MethodDeclaration
import eu.quanticol.carma.core.carma.MethodExpressions
import eu.quanticol.carma.core.carma.MethodReferenceMethodDeclaration
import eu.quanticol.carma.core.carma.MethodReferencePredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.NCA
import eu.quanticol.carma.core.carma.NewComponentArgumentDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentReference
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnDeclare
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnMethod
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnPrimitive
import eu.quanticol.carma.core.carma.NewComponentArgumentSpawnReference
import eu.quanticol.carma.core.carma.OutputActionArguments
import eu.quanticol.carma.core.carma.PredefinedMethodDeclaration
import eu.quanticol.carma.core.carma.PrimitiveType
import eu.quanticol.carma.core.carma.Probability
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.Rate
import eu.quanticol.carma.core.carma.RecordDeclaration
import eu.quanticol.carma.core.carma.RecordReferenceGlobal
import eu.quanticol.carma.core.carma.RecordReferenceMy
import eu.quanticol.carma.core.carma.RecordReferencePure
import eu.quanticol.carma.core.carma.RecordReferenceReceiver
import eu.quanticol.carma.core.carma.RecordReferenceSender
import eu.quanticol.carma.core.carma.RecordReferenceThis
import eu.quanticol.carma.core.carma.Records
import eu.quanticol.carma.core.carma.Spawn
import eu.quanticol.carma.core.carma.System
import eu.quanticol.carma.core.carma.Update
import eu.quanticol.carma.core.carma.VariableDeclaration
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaDouble
import eu.quanticol.carma.core.carma.VariableDeclarationCarmaIntger
import eu.quanticol.carma.core.carma.VariableDeclarationEnum
import eu.quanticol.carma.core.carma.VariableDeclarationRecord
import eu.quanticol.carma.core.carma.VariableReference
import eu.quanticol.carma.core.carma.VariableReferenceGlobal
import eu.quanticol.carma.core.carma.VariableReferenceMy
import eu.quanticol.carma.core.carma.VariableReferencePure
import eu.quanticol.carma.core.carma.VariableReferenceReceiver
import eu.quanticol.carma.core.carma.VariableReferenceSender
import eu.quanticol.carma.core.carma.VariableReferenceThis
import eu.quanticol.carma.core.carma.VariableType
import eu.quanticol.carma.core.carma.VariableTypeCarmaDouble
import eu.quanticol.carma.core.carma.VariableTypeCarmaIntger
import eu.quanticol.carma.core.carma.VariableTypeEnum
import eu.quanticol.carma.core.carma.VariableTypeRecord
import eu.quanticol.carma.core.generator.actions.ActionManager
import eu.quanticol.carma.core.generator.actions.NullBooleanExpression
import eu.quanticol.carma.core.generator.actions.NullInputActionArguments
import eu.quanticol.carma.core.generator.actions.NullOutputActionArguments
import eu.quanticol.carma.core.generator.actions.NullUpdate
import eu.quanticol.carma.core.generator.carmavariable.CarmaVariableManager
import eu.quanticol.carma.core.generator.components.ComponentManager
import eu.quanticol.carma.core.generator.measures.MeasureManager
import eu.quanticol.carma.core.typing.TypeProvider
import eu.quanticol.carma.core.utils.LabelUtil
import eu.quanticol.carma.core.utils.Util
import java.util.ArrayList
import java.util.HashMap

import static extension org.eclipse.xtext.EcoreUtil2.*

class GeneratorUtils {
	
	@Inject extension TypeProvider
	@Inject extension LabelUtil
	@Inject extension Util
	@Inject extension ExpressionHandler


	def String getStore(String carma_name, String carma_type, String java_assign, String store, String booleanFail){
		'''
		if(«store».get(«carma_name»,«carma_type») != null){
			«java_assign» = «store».get(«carma_name»,«carma_type»);
		} else {
			«booleanFail» = false;
		}
		'''
	}
	
	def String setStore(String carma_name, String carma_type, String java_assign, String store, String assign){
		'''
		if(«store».get(«carma_name»,«carma_type») != null){
			«store».set(«carma_name»,«assign»);
		}
		'''
	}
	
	def void populateCarmaVariableManager(CarmaVariableManager cvm, Model model){
		for(vd : model.eAllOfType(VariableDeclaration)){
			if(vd.getContainerOfType(MeasureBlock) == null){
				var isGlobal = vd.getContainerOfType(Environment) != null
				switch(vd){
					VariableDeclarationEnum:		cvm.loadDeclaration(vd.name.label,true,false,vd.hashCode,vd.assign,isGlobal)
					VariableDeclarationRecord:{
							var rds = vd.recordDeclarations
							for(rd : rds){
								switch(rd.assign){
									EnumAssignmentRange: cvm.loadDeclaration(vd.name.label+"_"+rd.name.label,true,true,rd.hashCode,rd,isGlobal)
									default: cvm.loadDeclaration(vd.name.label+"_"+rd.name.label,true,true,rd.hashCode,rd.assign,isGlobal)
								}
								
							}
					}
					VariableDeclarationCarmaDouble:	cvm.loadDeclaration(vd.name.label,false,false,vd.hashCode,vd.assign,isGlobal)
					VariableDeclarationCarmaIntger: cvm.loadDeclaration(vd.name.label,true,false,vd.hashCode,vd.assign,isGlobal)
				}
			}
		}
		for(vt : model.eAllOfType(VariableType)){
			switch(vt){
				VariableTypeEnum:			cvm.loadType(vt.name.label,true,false,vt.hashCode,null)
				VariableTypeRecord:			cvm.loadType(vt.name.label,true,true,vt.hashCode,null)
				VariableTypeCarmaDouble:	cvm.loadType(vt.name.label,false,false,vt.hashCode,null)
				VariableTypeCarmaIntger:	cvm.loadType(vt.name.label,true,false,vt.hashCode,null)
			}
		}
		for(vr : model.eAllOfType(VariableReference)){
			switch(vr){
				VariableReferencePure:		cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"")
				VariableReferenceMy:		cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"my_")
				VariableReferenceThis:		cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"this_")
				VariableReferenceReceiver:	cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"receiver_")
				VariableReferenceSender:	cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"sender_")
				VariableReferenceGlobal:	cvm.loadReference(vr.name.label,true,false,vr.hashCode,null,"global_")
				RecordReferencePure:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"")
				RecordReferenceMy:			cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"my_")
				RecordReferenceThis:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"this_")
				RecordReferenceReceiver:	cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"receiver_")
				RecordReferenceSender:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"sender_")
				RecordReferenceGlobal:		cvm.loadReference(vr.name.label+"_"+vr.record.label,true,true,vr.hashCode,null,"global_")
			}
		}
		cvm.loadModelName(model.label);
	}
	
	def HashMap<String,Integer> getFullNamesAndHash(VariableDeclaration vd){
		var HashMap<String,Integer> output = new HashMap<String,Integer>()
		if(vd.getContainerOfType(MeasureBlock) == null){
				switch(vd){
					VariableDeclarationEnum:		output.put(vd.name.label,vd.hashCode)
					VariableDeclarationRecord:{
							var rds = vd.recordDeclarations
							for(rd : rds)
								output.put(vd.name.label+"_"+rd.name.label,rd.hashCode)
					}
					VariableDeclarationCarmaDouble:	output.put(vd.name.label,vd.hashCode)
					VariableDeclarationCarmaIntger: output.put(vd.name.label,vd.hashCode)
				}
			}
		return output
	}
	
	def BooleanExpressions getBooleanExpression(Action action){
		if(action.eAllOfType(ActionGuard).size > 0){
			return action.eAllOfType(ActionGuard).get(0).booleanExpression
		} else {
			return new NullBooleanExpression();
		}
	}
	
	def Update getUpdate(Action action){
		if(action.eAllOfType(Update).size > 0){
			return action.eAllOfType(Update).get(0)
		} else {
			return new NullUpdate();
		}
	}
	
	def OutputActionArguments getOutputActionArguments(Action action){
		if(action.eAllOfType(OutputActionArguments).size > 0){
			return action.eAllOfType(OutputActionArguments).get(0)
		} else {
			return new NullOutputActionArguments();
		}
	}
	
	def InputActionArguments getInputActionArguments(Action action){
		if(action.eAllOfType(InputActionArguments).size > 0){
			return action.eAllOfType(InputActionArguments).get(0)
		} else {
			return new NullInputActionArguments();
		}
	}
	
	def void populateActionManager(ActionManager am, Model model){
		
		var actions = model.eAllOfType(Action)
		var actionStubs = model.eAllOfType(ActionStub)
		
		for(action : actions){
			am.loadAction(action.labelIO, action.hashCode, model.label)
			am.loadPredicate(action.name.label,action.hashCode,action.booleanExpression)
			am.loadUpdate(action.name.label,action.hashCode,action.update)
			am.loadOutputActionArguments(action.name.label,action.hashCode,action.outputActionArguments)
			am.loadInputActionArguments(action.name.label,action.hashCode,action.inputActionArguments)
		}
			
		for(actionStub : actionStubs){
			var String type = ""
			var String guard = ""
			var String expression = ""
			var eo = actionStub.getContainerOfType(EnvironmentOperation)
			switch(eo){
				Rate:				{
					type = "rate" 
					guard = eo.guard.booleanExpression.disarmExpression.toUpperCase
					expression = eo.expression.evaluateExpression
				}
				EnvironmentUpdate:  {
					type = "upda"
					guard = eo.guard.booleanExpression.disarmExpression.toUpperCase
					expression = eo.expression.evaluateExpression
				}
				Probability:		{
					type = "prob"
					guard = eo.guard.booleanExpression.disarmExpression.toUpperCase
					expression = eo.expression.evaluateExpression
				}
			}
			am.loadStub(actionStub.name.label,type,guard,expression)
		}
	}
	
	def void populateComponentManager(ComponentManager cm, Model model){
		
		var components = model.eAllOfType(Component)
		
		for(component : components){
			var cas = new ArrayList<ComponentArgument>(component.eAllOfType(ComponentArgument))
			cm.loadComponent(component.label,component.getTree, cas.arguments, component.macros);
			var vds = component.eAllOfType(VariableDeclaration)
			for(vd : vds)
				cm.loadVDS(component.label,vd.fullNamesAndHash)
		}
		for(cbnd : model.eAllOfType(ComponentBlockNewDeclaration)){
			if(cbnd.getContainerOfType(BlockSystem) != null){
				var ncas = new ArrayList<NCA>(cbnd.eAllOfType(NCA))
				var arguments = ncas.stripArguments
				if(cbnd.getContainerOfType(ComponentBlockForStatement) != null){
					var cbfs = cbnd.getContainerOfType(ComponentBlockForStatement)
					var fvd = cbfs.eAllOfType(ForVariableDeclaration).get(0)
					var bes = cbfs.eAllOfType(BooleanExpressions).get(0)
					var vr = cbfs.eAllOfType(VariableReference).get(0)
					var mes = cbfs.eAllOfType(MethodExpressions).get(0)
					cm.loadNew(cbnd.name.label,arguments,true,fvd,bes,vr,mes)
				} else {
					cm.loadNew(cbnd.name.label,arguments,false,null,null,null,null)
				}
			}
		}
		for(spawn : model.eAllOfType(Spawn)){
			if(spawn.eAllOfType(BlockSpawn).size > 0){
				if(spawn.eAllOfType(ComponentBlockNewDeclarationSpawn).size > 0){
					for(cbnds : spawn.eAllOfType(ComponentBlockNewDeclarationSpawn)){
						var ncas = new ArrayList<NCA>(cbnds.eAllOfType(NCA))
						var arguments = ncas.stripArguments
						cm.loadSpawn(cbnds.name.label,arguments,cbnds.hashCode)
					}
				}
			}
		}
	}
	
	def ArrayList<String> getArguments(ArrayList<ComponentArgument> cas){
		var ArrayList<String> output = new ArrayList<String>()		
		for(ca : cas){
			switch(ca){
				ComponentBlockDefinitionArgumentVariable: ca.value.takeArguments(output)
			}
		}
		return output
	}
	
	def void takeArguments(VariableType vt, ArrayList<String> output){
		switch(vt){
			VariableTypeEnum:			output.add(vt.name.label) 
			VariableTypeRecord:			vt.spread(output)
			VariableTypeCarmaDouble:	output.add(vt.name.label)
			VariableTypeCarmaIntger:	output.add(vt.name.label)
		}
	}
	
	def void spread(VariableTypeRecord vtr, ArrayList<String> output){
		
		var ArrayList<RecordDeclaration> rds = new ArrayList<RecordDeclaration>()
		var position = vtr.getPosition
		var cbnds = vtr.getComponentToCBNDs
		
		for(cd : cbnds.keySet){
			for(c : cbnds.get(cd)){
				if(c.getContainerOfType(ComponentBlockStyleCollective) != null){
					rds.addAll((c as ComponentBlockNewDeclaration).componentInputArguments.inputArguments.get(position).eAllOfType(RecordDeclaration))
				} else if (c.getContainerOfType(EnvironmentUpdate) != null) {
					if((c as ComponentBlockNewDeclarationSpawn).componentInputArguments.inputArguments.get(position).eAllOfType(NewComponentArgumentSpawnReference).size > 0){
						var vr = (c as ComponentBlockNewDeclarationSpawn).componentInputArguments.inputArguments.get(position).eAllOfType(NewComponentArgumentSpawnReference).get(0).value
						var vd = vr.variableDeclaration as VariableDeclarationRecord
						rds.addAll(vd.recordDeclarations) 
					}
					rds.addAll((c as ComponentBlockNewDeclarationSpawn).componentInputArguments.inputArguments.get(position).eAllOfType(RecordDeclaration))
				}
			}
		}
		
		for(var i = 0; i < rds.size; i++){
			output.add(rds.get(i).name.label)
		}
	}
	
	def ArrayList<ArrayList<String>> stripArguments(ArrayList<NCA> ncas){
		var ArrayList<ArrayList<String>> output = new ArrayList<ArrayList<String>>()		
		for(argument : ncas){
			if(argument.type.toString.equals("component")){
				argument.takeArguments(output)
			}
		}
		return output
	}
	
	def void takeArguments(NCA nca,ArrayList<ArrayList<String>> output){
		switch(nca){
			NewComponentArgumentPrimitive: 	nca.value.takeArguments(output)
			NewComponentArgumentDeclare: 	nca.value.takeArguments(output)
			NewComponentArgumentMethod: 	nca.value.takeArguments(output)
			NewComponentArgumentReference: 	nca.value.takeArguments(output)
			NewComponentArgumentSpawnPrimitive: 	nca.value.takeArguments(output)
			NewComponentArgumentSpawnDeclare: 		nca.value.takeArguments(output)
			NewComponentArgumentSpawnMethod: 		nca.value.takeArguments(output)
			NewComponentArgumentSpawnReference: 	nca.value.takeArguments(output)
		}
	}
	
	def void takeArguments(VariableReference vr, ArrayList<ArrayList<String>> output){
		var temp = new ArrayList<String>()
		//because there will only be one kind of reference @ spawn
		temp.add(vr.asJavaEvolutionRule)
		output.add(temp)
	}
	
	def void takeArguments(MethodExpressions e, ArrayList<ArrayList<String>> output){
		switch(e){
			MethodReferenceMethodDeclaration: 			{
				var temp = new ArrayList<String>()
				temp.add((e.ref as MethodDeclaration).asJava)
				output.add(temp)
				}
			MethodReferencePredefinedMethodDeclaration: {
				var temp = new ArrayList<String>()
				temp.add((e.ref as PredefinedMethodDeclaration).asJava)
				output.add(temp)
				}
		}
	}
	
	def void takeArguments(PrimitiveType pt, ArrayList<ArrayList<String>> output){
		switch(pt){
			CarmaInteger:	{
				var temp = new ArrayList<String>()
				temp.add(""+pt.value)
				output.add(temp)
				
				}
			Range:{
				var temp = new ArrayList<String>()
				for(var i = pt.min; i <= pt.max; i++){
					temp.add(""+i)
				}
				output.add(temp)
			}
		}
	}
	
	def void takeArguments(Records records, ArrayList<ArrayList<String>> output){
		for(rec : records.eAllOfType(RecordDeclaration)){
			switch(rec.assign){
				EnumAssignmentCarmaInteger:  	{
					var temp = new ArrayList<String>()
					temp.add((rec.assign as EnumAssignmentCarmaInteger).naturalValue.asJava)
					output.add(temp)
					}
				EnumAssignmentMethodReference:	{
					var temp = new ArrayList<String>()
					temp.add((rec.assign as EnumAssignmentMethodReference).method.asJava)
					output.add(temp)
					}
				EnumAssignmentRange: 	{
					var temp = new ArrayList<String>()
					var pt = ((rec.assign as EnumAssignmentRange).range as Range)
					for(var i = pt.min; i <= pt.max; i++){
						temp.add(""+i)
					}
					output.add(temp)
				}
			}
		}
	}
	
	def void populateMeasureManager(MeasureManager mm, Model model){
		
		var measures = model.eAllOfType(Measure)
		var environmentMeasures = model.eAllOfType(EnvironmentMeasure);
		
		for(measure : measures){
			var measureName = measure.name.getLabel.toFirstUpper
			var stateName = (measure.measure as EnvironmentMeasure).componentReference.getLabel.toFirstUpper
			mm.loadMeasure(measureName,stateName,measure.parameters.args, measure.measure);
		}
		
		for(environmentMeasure : environmentMeasures)
			if(environmentMeasure.getContainerOfType(Environment) != null){
				mm.loadSystemMeasure(environmentMeasure.getContainerOfType(System).label,environmentMeasure.disarmExpression)
				println(mm.getMeasures(environmentMeasure.getContainerOfType(System).label))
				mm.loadEnvMeasure(environmentMeasure.disarmExpression,environmentMeasure)
			}
		
	}

	def HashMap<String,ArrayList<String>> getArgs(MeasureVariableDeclarations mvds){
		var HashMap<String,ArrayList<String>> output = new HashMap<String,ArrayList<String>>()
		for(vd : mvds.eAllOfType(EnumAssignment)){
			switch(vd){
				EnumAssignmentCarmaInteger: { 
					var temp = new ArrayList<String>() 
					temp.add(vd.naturalValue.asJava) 
					output.put(vd.getContainerOfType(VariableDeclaration).name.label,temp)
				}
				EnumAssignmentRange: {
					var temp = new ArrayList<String>()
					for(var i = (vd.range as Range).min; i <= (vd.range as Range).max; i++){
						temp.add(i+"")
					}
					output.put(vd.getContainerOfType(VariableDeclaration).name.label,temp)
				}
			}
		}
		return output
	}
	
}