package eu.quanticol.carma.core

import eu.quanticol.carma.core.carma.Model
import eu.quanticol.carma.core.carma.FunctionDefinition
import eu.quanticol.carma.core.carma.RecordDefinition
import eu.quanticol.carma.core.carma.Processes
import eu.quanticol.carma.core.carma.ConstantDefinition
import eu.quanticol.carma.core.carma.ComponentDefinition
import eu.quanticol.carma.core.carma.EnumDefinition
import eu.quanticol.carma.core.carma.MeasureDefinition
import eu.quanticol.carma.core.carma.SystemDefinition
import eu.quanticol.carma.core.carma.CollectiveDefinition
import eu.quanticol.carma.core.carma.SpaceDefinition
import eu.quanticol.carma.core.carma.BasicType
import eu.quanticol.carma.core.carma.IntegerType
import eu.quanticol.carma.core.carma.LocationType
import eu.quanticol.carma.core.carma.BooleanType
import eu.quanticol.carma.core.carma.CustomType
import eu.quanticol.carma.core.carma.ReferenceableType
import eu.quanticol.carma.core.carma.ListType
import eu.quanticol.carma.core.carma.SetType
import eu.quanticol.carma.core.carma.RealType
import eu.quanticol.carma.core.carma.Variable
import eu.quanticol.carma.core.carma.ValueType
import eu.quanticol.carma.core.carma.ProcessType
import eu.quanticol.carma.core.carma.IfThenElseCommand
import eu.quanticol.carma.core.carma.ReturnCommand
import eu.quanticol.carma.core.carma.VariableDeclarationCommand
import eu.quanticol.carma.core.carma.ForLoop
import eu.quanticol.carma.core.carma.ForCommand
import eu.quanticol.carma.core.carma.ForEach
import eu.quanticol.carma.core.carma.BlockCommand
import eu.quanticol.carma.core.carma.AssignmentCommand
import eu.quanticol.carma.core.carma.Or
import eu.quanticol.carma.core.carma.And
import eu.quanticol.carma.core.carma.Equality
import eu.quanticol.carma.core.carma.DisEquality
import eu.quanticol.carma.core.carma.Less
import eu.quanticol.carma.core.carma.LessOrEqual
import eu.quanticol.carma.core.carma.Greater
import eu.quanticol.carma.core.carma.GreaterOrEqual
import eu.quanticol.carma.core.carma.IsIn
import eu.quanticol.carma.core.carma.Subtraction
import eu.quanticol.carma.core.carma.Addition
import eu.quanticol.carma.core.carma.Multiplication
import eu.quanticol.carma.core.carma.Division
import eu.quanticol.carma.core.carma.Modulo
import eu.quanticol.carma.core.carma.IfThenElseExpression
import eu.quanticol.carma.core.carma.Not
import eu.quanticol.carma.core.carma.UnaryPlus
import eu.quanticol.carma.core.carma.UnaryMinus
import eu.quanticol.carma.core.carma.FieldAccess
import eu.quanticol.carma.core.carma.NodeExpressionOrArrayAccess
import eu.quanticol.carma.core.carma.PreSetExpression
import eu.quanticol.carma.core.carma.PoSetExpression
import eu.quanticol.carma.core.carma.Reference
import eu.quanticol.carma.core.carma.SetExpression
import eu.quanticol.carma.core.carma.ListExpression
import eu.quanticol.carma.core.carma.LambdaParameter
import eu.quanticol.carma.core.carma.Locations
import eu.quanticol.carma.core.carma.MyLocation
import eu.quanticol.carma.core.carma.AtomicTrue
import eu.quanticol.carma.core.carma.AtomicFalse
import eu.quanticol.carma.core.carma.AtomicInteger
import eu.quanticol.carma.core.carma.AtomicReal
import eu.quanticol.carma.core.carma.AtomicRecord
import eu.quanticol.carma.core.carma.FieldAssignment
import eu.quanticol.carma.core.carma.AtomicNow
import eu.quanticol.carma.core.carma.None
import eu.quanticol.carma.core.carma.SetComp
import eu.quanticol.carma.core.carma.ComponentComprehension
import eu.quanticol.carma.core.carma.AllComponents
import eu.quanticol.carma.core.carma.AComponentAllStates
import eu.quanticol.carma.core.carma.AComponentAState
import eu.quanticol.carma.core.carma.ParallelComposition
import eu.quanticol.carma.core.carma.ProcessReference
import eu.quanticol.carma.core.carma.AtomicPi
import eu.quanticol.carma.core.carma.AtomicExp
import eu.quanticol.carma.core.carma.NormalSampling
import eu.quanticol.carma.core.carma.AbsFunction
import eu.quanticol.carma.core.carma.AcosFunction
import eu.quanticol.carma.core.carma.AsinFunction
import eu.quanticol.carma.core.carma.AtanFunction
import eu.quanticol.carma.core.carma.Atan2Function
import eu.quanticol.carma.core.carma.CbrtFunction
import eu.quanticol.carma.core.carma.CeilFunction
import eu.quanticol.carma.core.carma.CosFunction
import eu.quanticol.carma.core.carma.ExpFunction
import eu.quanticol.carma.core.carma.FloorFunction
import eu.quanticol.carma.core.carma.LogFunction
import eu.quanticol.carma.core.carma.Log10Function
import eu.quanticol.carma.core.carma.MaxFunction
import eu.quanticol.carma.core.carma.MaxMeasure
import eu.quanticol.carma.core.carma.MinFunction
import eu.quanticol.carma.core.carma.MinMeasure
import eu.quanticol.carma.core.carma.AverageMeasure
import eu.quanticol.carma.core.carma.PowFunction
import eu.quanticol.carma.core.carma.SinFunction
import eu.quanticol.carma.core.carma.SqrtFunction
import eu.quanticol.carma.core.carma.TanFunction
import eu.quanticol.carma.core.carma.PreFunction
import eu.quanticol.carma.core.carma.PostFunction
import eu.quanticol.carma.core.carma.UniformFunction
import eu.quanticol.carma.core.carma.AccessToEdgeValue
import eu.quanticol.carma.core.carma.NewListFunction
import eu.quanticol.carma.core.carma.NewSetFunction
import eu.quanticol.carma.core.carma.SizeFunction
import eu.quanticol.carma.core.carma.MapFunction
import eu.quanticol.carma.core.carma.FilterFunction
import eu.quanticol.carma.core.carma.ExistsFunction
import eu.quanticol.carma.core.carma.ForAllFunction
import eu.quanticol.carma.core.carma.SelectFunction
import eu.quanticol.carma.core.carma.AssignmentTarget
import eu.quanticol.carma.core.carma.TargetAssignmentField
import eu.quanticol.carma.core.carma.TargetAssignmentVariable
import eu.quanticol.carma.core.carma.ProcessExpressionChoice
import eu.quanticol.carma.core.carma.ProcessExpressionGuard
import eu.quanticol.carma.core.carma.ProcessExpressionAction
import eu.quanticol.carma.core.carma.ProcessExpressionNext
import eu.quanticol.carma.core.carma.ProcessExpressionNil
import eu.quanticol.carma.core.carma.ProcessExpressionKill
import eu.quanticol.carma.core.carma.ProcessExpressionReference
import eu.quanticol.carma.core.carma.Action
import eu.quanticol.carma.core.carma.OutputAction
import eu.quanticol.carma.core.carma.InputAction
import eu.quanticol.carma.core.carma.Activity
import eu.quanticol.carma.core.carma.UpdateCommand
import eu.quanticol.carma.core.carma.MyContext
import eu.quanticol.carma.core.carma.GlobalContext
import eu.quanticol.carma.core.carma.AttributeReference
import eu.quanticol.carma.core.carma.StoreAttribute
import eu.quanticol.carma.core.carma.LocAttribute
import eu.quanticol.carma.core.carma.UpdateCollectionAdd
import eu.quanticol.carma.core.carma.UpdateAssignment
import eu.quanticol.carma.core.carma.UpdateCollectionRemove
import eu.quanticol.carma.core.carma.ReceiverContext
import eu.quanticol.carma.core.carma.SenderContext
import eu.quanticol.carma.core.carma.CastToInteger
import eu.quanticol.carma.core.carma.CastToReal
import eu.quanticol.carma.core.carma.TupleExpression
import eu.quanticol.carma.core.carma.AtomicRnd
import eu.quanticol.carma.core.carma.AttributeDeclaration
import eu.quanticol.carma.core.carma.AttributeConstDeclaration
import eu.quanticol.carma.core.carma.AttibuteVarDeclaration
import eu.quanticol.carma.core.carma.ProcessState
import eu.quanticol.carma.core.carma.SystemCollective
import eu.quanticol.carma.core.carma.CollectiveReference
import eu.quanticol.carma.core.carma.CollectiveBlock
import eu.quanticol.carma.core.carma.ComponentBlockInstantiation
import eu.quanticol.carma.core.carma.ComponentBlockFor
import eu.quanticol.carma.core.carma.ComponentBlockConditionalStatement
import eu.quanticol.carma.core.carma.ComponentBlockForStatement
import eu.quanticol.carma.core.carma.ComponentBlockIteratorStatement
import eu.quanticol.carma.core.carma.Environment
import eu.quanticol.carma.core.carma.StoreBlock
import eu.quanticol.carma.core.carma.ProbabilityBlock
import eu.quanticol.carma.core.carma.WeightBlock
import eu.quanticol.carma.core.carma.RateBlock
import eu.quanticol.carma.core.carma.UpdateBlock
import eu.quanticol.carma.core.carma.ActionStub
import eu.quanticol.carma.core.carma.Range
import eu.quanticol.carma.core.carma.UniverseElement
import eu.quanticol.carma.core.carma.NodeBodyCommand
import eu.quanticol.carma.core.carma.ConnectionBodyCommand
import eu.quanticol.carma.core.carma.NodeDeclaration
import eu.quanticol.carma.core.carma.NamedNode
import eu.quanticol.carma.core.carma.UnNamedNode
import eu.quanticol.carma.core.carma.NodeIfThenElseCommand
import eu.quanticol.carma.core.carma.NodeForCommand
import eu.quanticol.carma.core.carma.NodeBlockCommand
import eu.quanticol.carma.core.carma.NodeForEach
import eu.quanticol.carma.core.carma.LoopingVariable
import eu.quanticol.carma.core.carma.ConnectionForEach
import eu.quanticol.carma.core.carma.ConnectionForCommand
import eu.quanticol.carma.core.carma.ConnectionBlockCommand
import eu.quanticol.carma.core.carma.ConnectionIfThenElseCommand
import eu.quanticol.carma.core.carma.ConnectionDeclaration
import eu.quanticol.carma.core.carma.UnNamedLocationExpression
import eu.quanticol.carma.core.carma.NamedLocationExpression
import eu.quanticol.carma.core.carma.Direction
import eu.quanticol.carma.core.carma.DirectedEdge
import eu.quanticol.carma.core.carma.UnDirectedEdge
import eu.quanticol.carma.core.carma.EdgeProperty
import eu.quanticol.carma.core.carma.AreaElementDeclaration
import eu.quanticol.carma.core.carma.AreaIfThenElseCommand
import eu.quanticol.carma.core.carma.AreaBlockCommand
import eu.quanticol.carma.core.carma.AreaForCommand
import eu.quanticol.carma.core.carma.AreaForEach
import java.math.BigDecimal
import java.text.DecimalFormat
import eu.quanticol.carma.core.carma.UpdateArrayElement
import eu.quanticol.carma.core.carma.TargetAssignmentList

class CarmaModelToCode {
	
	def modelToCode( Model m ) {
		'''
		«FOR e:m.elements»
		
		«e.elementToCode»
		
		«ENDFOR»
		'''
	}
	
	def dispatch elementToCode( FunctionDefinition f ) {
		'''
		fun «f.type.typeToCode» «f.name» («FOR p:f.parameters SEPARATOR ', '»«p.variableToCode»«ENDFOR») «f.body.functionCommandToCode»
		'''
	}
	
	def dispatch CharSequence functionCommandToCode( IfThenElseCommand c ) {
		'''
		if («c.condition.expressionToCode») {
			«c.thenBlock.functionCommandToCode»	
		}«IF c.elseBlock!=null» else {
			«c.elseBlock.functionCommandToCode»
		}
		«ENDIF»
		'''	
	}

	def dispatch CharSequence functionCommandToCode( ReturnCommand c ) {
		'''return «c.expression.expressionToCode»;'''	
	}

	def dispatch CharSequence functionCommandToCode( VariableDeclarationCommand c ) {
		'''«c.variable.variableToCode»«IF c.value != null» = «c.value.expressionToCode»«ENDIF»;'''		
	}

	def dispatch CharSequence functionCommandToCode( ForCommand c ) {
		'''
		for «c.variable.name» from «c.start.expressionToCode»«IF c.step != null» by «c.step.expressionToCode»«ENDIF» to «c.end.expressionToCode» «c.body.functionCommandToCode»
		'''
	}

	def dispatch CharSequence functionCommandToCode( ForEach c ) {
		'''
		for «c.iteration.name» in «c.iteration.value.expressionToCode» «c.body.functionCommandToCode»
		'''
	}

	def dispatch CharSequence functionCommandToCode( BlockCommand c ) {
		'''
		{
			«FOR c1:c.commands»
			«c1.functionCommandToCode»
			«ENDFOR»
		}
		'''
	}

	def dispatch CharSequence functionCommandToCode( AssignmentCommand c ) {
		'''
		«c.target.assignmentTargetToCode» = «c.value.expressionToCode»;
		'''		
	}
	
	def dispatch CharSequence assignmentTargetToCode( TargetAssignmentField f ) {
		'''«f.target.assignmentTargetToCode».«f.field.name»'''
	}

	def dispatch CharSequence assignmentTargetToCode( TargetAssignmentVariable f ) {
		'''«f.variable.name»'''
	}

	def dispatch CharSequence assignmentTargetToCode( TargetAssignmentList f ) {
		'''«f.target.assignmentTargetToCode»[ «f.index.expressionToCode» ]'''
	}

	def CharSequence variableToCode( Variable v ) {
		'''«v.type.valueTypeToCode» «v.name»'''
	}
	
	def CharSequence loopingVariableToCode( LoopingVariable v) {
		'''«v.name» in «v.value.expressionToCode»'''
	}
	
	def CharSequence valueTypeToCode( ValueType v ) {
		switch v {
			ProcessType: '''process'''
			BasicType: v.typeToCode
		}
	} 

	def CharSequence typeToCode( BasicType type ) {
		switch type {
			IntegerType: '''int'''
			LocationType: '''location'''
			BooleanType: '''bool'''
			CustomType: '''«type.reference.referenceableTypeToCode»'''
			ListType: '''list<«type.arg.typeToCode»>'''
			SetType: '''set<«type.arg.typeToCode»>'''
			RealType: '''real'''
		}		
	}

	def CharSequence referenceableTypeToCode( ReferenceableType type ) {
		switch type {
			EnumDefinition: type.name
			RecordDefinition: type.name
		}
	}

	def dispatch elementToCode( RecordDefinition r ) {
		'''
		record «r.name» = [
			«FOR f:r.fields SEPARATOR ','»
			«f.fieldType.typeToCode» «f.name»
			«ENDFOR»		
		];
		'''		
	}
	
	def dispatch elementToCode( Processes p ) {
		'''
		abstract {
			«FOR s:p.processes»
			«s.processStateToCode»
			«ENDFOR»
		}
		'''
	}
	
	def processStateToCode( ProcessState s ) {
		'''
		«s.name» = «s.processExpression.processExpressionToCode»;
		'''
	}
	
	def dispatch CharSequence processExpressionToCode( ProcessExpressionChoice p ) {
		'''(«p.left.processExpressionToCode»+«p.right.processExpressionToCode»)'''
	}
	
	def dispatch CharSequence processExpressionToCode( ProcessExpressionGuard p ) {
		'''[ «p.guard.booleanExpression.expressionToCode» ] «p.expression.processExpressionToCode»'''		
	}
	
	def dispatch CharSequence processExpressionToCode( ProcessExpressionAction p ) {
		'''«p.action.actionToCode».«p.next.processExpressionNextToCode»'''		
	}
	
	def processExpressionNextToCode( ProcessExpressionNext p ) {
		switch p {
			ProcessExpressionNil: '''nil'''	
			ProcessExpressionKill: '''kill'''
			ProcessExpressionReference: '''«p.expression.name»'''
		}
	}
	
	def dispatch CharSequence actionToCode( OutputAction a ) {
		'''«a.activity.actionToCode»
				«IF a.withData»< «FOR e:a.outputArguments SEPARATOR ','» «e.expressionToCode» «ENDFOR»>«ENDIF»
				«IF a.update != null»{
					«FOR u:a.update.updateAssignment»
					«u.updateCommandToCode»
					«ENDFOR»
				}«ENDIF»
		'''		
	}
	
	def dispatch CharSequence attributeTargetToCode( MyContext at ) {
		'''my.«at.attribute.attributeTargetToCode»'''
	}
	
	def dispatch CharSequence attributeTargetToCode( GlobalContext at ) {
		'''global.«at.reference.name»'''
	}
	
	def dispatch CharSequence attributeTargetToCode( AttributeReference at ) {
		switch at {
			StoreAttribute: '''«at.reference.name»'''
			LocAttribute: '''loc'''
		}
	}
	
	def dispatch CharSequence updateCommandToCode( UpdateAssignment c ) {
		'''«c.target.attributeTargetToCode» = «c.expression.expressionToCode»;'''
	}

	def dispatch CharSequence updateCommandToCode( UpdateCollectionAdd c ) {
		'''«c.target.attributeTargetToCode».add(«c.expression.expressionToCode»);'''
	}
	
	def dispatch CharSequence updateCommandToCode( UpdateCollectionRemove c ) {
		'''«c.target.attributeTargetToCode».remove(«c.expression.expressionToCode»);'''
	}
	
	def dispatch CharSequence updateCommandToCode(UpdateArrayElement c ) {
		'''«c.target.attributeTargetToCode»«FOR i : c.indexes»[ «i.expressionToCode» ]«ENDFOR» = «c.expression.expressionToCode»;'''
	}
	
	
	def dispatch CharSequence actionToCode( InputAction a ) {
		'''«a.activity.actionToCode»
				( «FOR v:a.parameters SEPARATOR ','» «v.name» «ENDFOR»)
				«IF a.update != null»{
					«FOR u:a.update.updateAssignment»
					«u.updateCommandToCode»
					«ENDFOR»
				}«ENDIF»
		'''		
	}
	
	def dispatch CharSequence actionToCode( Activity a ) { 
		'''«a.name»«IF a.isIsBroadacst»*«ENDIF»«IF a.predicate != null»[«a.predicate.guard.expressionToCode»]«ENDIF»'''	
	}
	
	def dispatch elementToCode( ConstantDefinition f ) {
		'''const «f.name» = «f.value.expressionToCode»;'''	
	}	
	
	def dispatch elementToCode( ComponentDefinition c ) {
		'''
		component «c.name» ( «FOR p:c.parameters SEPARATOR ','»«p.variableToCode»«ENDFOR» ) {
			«c.store.storeToCode»
			behaviour {
				«FOR s:c.processes.processes»
				«s.processStateToCode»
				«ENDFOR»	
			}
			init {
				«c.initBlock.init.processCompositionToCode»	
			}
		}		
		'''
	}
	
	def storeToCode( StoreBlock store ) {
		'''
		store {
			«FOR a:store.attributes»
			«a.attributeDeclarationToCode»
			«ENDFOR»
		}
		'''
	}
	
	def attributeDeclarationToCode( AttributeDeclaration a ) {
		switch a {
			AttributeConstDeclaration: '''const «a.name» = «a.value.expressionToCode»;'''
			AttibuteVarDeclaration: '''attrib «IF a.type!=null»«a.type.typeToCode»«ENDIF» «a.name» = «a.value.expressionToCode»;'''
		}
	} 
	
	def dispatch elementToCode( EnumDefinition f ) {
		'''enum «f.name» = «FOR c:f.values  SEPARATOR ','»«c.name»«ENDFOR»;'''	
	}
	
	def dispatch elementToCode( MeasureDefinition f ) {
		'''measure «f.name»«IF !f.variables.empty»( «FOR v:f.variables SEPARATOR ','»«v.variableToCode»«ENDFOR» )«ENDIF» = «f.measure.expressionToCode»;'''	
	}
	
	def dispatch elementToCode( SystemDefinition f ) {
		'''
		system «f.name» {
			«IF f.space != null»space «f.space.name»(«FOR e:f.args SEPARATOR ','»«e.expressionToCode»«ENDFOR»)«ENDIF»
			collective «f.collective.collectiveToCode»	
			«IF f.environment != null»
			«f.environment.environmentToCode»
			«ENDIF»		
		}
		'''
	}
	
	def environmentToCode( Environment e ) {
		'''
		environment {
			«IF e.store != null»«e.store.storeToCode»«ENDIF»
			«IF e.probabilityBlock != null»«e.probabilityBlock.probabilityBlockToCode»«ENDIF»
			«IF e.weightBlock != null»«e.weightBlock.weightBlockToCode»«ENDIF»
			«IF e.rateBlock != null»«e.rateBlock.rateBlockToCode»«ENDIF»
			«IF e.updateBlock != null»«e.updateBlock.updateBlockToCode»«ENDIF»
		}		
		'''
	}
	
	
	def probabilityBlockToCode( ProbabilityBlock block ) {
		'''
		prob {
			«FOR p:block.probabilities»
			«p.activity.name»* «p.expression.functionCommandToCode»
			«ENDFOR»
			«IF block.value != null»
			default «block.value.functionCommandToCode»
			«ENDIF»
		}
		'''
	}

	def weightBlockToCode( WeightBlock block ) {
		'''
		weight {
			«FOR w:block.weights»
			«w.activity.name» «w.expression.functionCommandToCode»
			«ENDFOR»
			«IF block.value != null»
			default «block.value.functionCommandToCode»
			«ENDIF»
		}
		'''
	}
	
	def rateBlockToCode( RateBlock block ) {
		'''
		rate {
			«FOR w:block.rates»
			«w.stub.actionStubToCode» «w.expression.functionCommandToCode»
			«ENDFOR»
			«IF block.value != null»
			default «block.value.functionCommandToCode»
			«ENDIF»
		}
		'''
	}
	
	def updateBlockToCode( UpdateBlock block ) {
		'''
		update {
			«FOR w:block.updates»
			«w.stub.actionStubToCode» «w.command.collectiveToCode»
			«ENDFOR»
		}
		'''
	}
	
	def actionStubToCode( ActionStub stub ) {
		'''«stub.activity.name»«IF stub.isIsBroadcast»*«ENDIF»'''
	}
	
	
	def dispatch collectiveToCode( CollectiveReference collective ) {
		'''«collective.reference.name»'''
	}

	def dispatch collectiveToCode( CollectiveBlock collective ) {
		''' {
			«FOR c:collective.collective» 
			«c.collectiveBlockDeclarationToCode»
			«ENDFOR»		
		}
		'''
	}
	
	def dispatch CharSequence collectiveBlockDeclarationToCode( UpdateCommand c ) {
		c.updateCommandToCode
	}
	
	def dispatch CharSequence collectiveBlockDeclarationToCode( ComponentBlockInstantiation c ) {
		'''new «c.name.name»( «FOR e:c.arguments SEPARATOR ','»«e.expressionToCode»«ENDFOR» )«IF c.location!=null»@«c.location.expressionToCode»«ENDIF»«IF c.population!=null»< «c.population.expressionToCode» >«ENDIF»;'''
	}
	
	def dispatch CharSequence collectiveBlockDeclarationToCode( ComponentBlockForStatement c ) {
		'''
		for ( «c.variable.name» ; «c.expression.expressionToCode» ; «c.afterThought.expressionToCode» ) {
			«FOR c1:c.collective»
			«c1.collectiveBlockDeclarationToCode»
			«ENDFOR»			
		}
		'''	
	}
		
	def dispatch CharSequence collectiveBlockDeclarationToCode( ComponentBlockIteratorStatement c ) {
		'''
		for «c.iteration.name» in «c.iteration.value.expressionToCode» {
			«FOR c1:c.collective»
			«c1.collectiveBlockDeclarationToCode»
			«ENDFOR»			
		}
		'''			
	}
	
	def dispatch CharSequence collectiveBlockDeclarationToCode( ComponentBlockConditionalStatement c ) {
		'''
		if ( «c.guard.expressionToCode» ) {
			«FOR c1:c.thenBranch»
			«c1.collectiveBlockDeclarationToCode»			
			«ENDFOR»
		} «IF c.elseBranch != null» else {
			«FOR c1:c.elseBranch»
			«c1.collectiveBlockDeclarationToCode»			
			«ENDFOR»
		}	
		«ENDIF»
		'''	
	}
	
	def dispatch elementToCode( CollectiveDefinition f ) {
		'''
		collective «f.name» «f.block.collectiveToCode»
		'''		
	}
	
	
	//TODO:	
	def dispatch elementToCode( SpaceDefinition f ) {
		'''
		space «f.name» ( «FOR p:f.parameters SEPARATOR ', '»«p.variableToCode»«ENDFOR» ) {
			universe <«FOR e:f.universe SEPARATOR ', '»«e.universeElementToCode»«ENDFOR»>
			nodes {
				«FOR n : f.nodes»
				«n.nodeBodyToCode»
				«ENDFOR»
			}
			connections {
				«FOR c : f.edges»«c.connectionBodyToCode»«ENDFOR»
			}
			areas {
				«FOR l : f.labels»
				«l.name» {
					«FOR n : l.nodes»«n.areaBodyToCode»«ENDFOR»
				}
				«ENDFOR»
			}
		}
		'''
	}
	
	def CharSequence universeElementToCode( UniverseElement e) {
		'''«e.type.valueTypeToCode» «e.name»'''
	}
	
	def dispatch CharSequence expressionToCode( Or e ) {
		'''( «e.left.expressionToCode» || «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( And e ) {
		'''( «e.left.expressionToCode» && «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( Equality e ) {
		'''( «e.left.expressionToCode» == «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( DisEquality e ) {
		'''( «e.left.expressionToCode» != «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( Less e ) {
		'''( «e.left.expressionToCode» < «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( LessOrEqual e ) {
		'''( «e.left.expressionToCode» <= «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( Greater e ) {
		'''( «e.left.expressionToCode» > «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( GreaterOrEqual e ) {
		'''( «e.left.expressionToCode» >= «e.right.expressionToCode» )'''
	}

	def dispatch CharSequence expressionToCode( IsIn e ) {
		'''( «e.left.expressionToCode» in «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( Subtraction e ) {
		'''( «e.left.expressionToCode» - «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( Addition e ) {
		'''( «e.left.expressionToCode» + «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( Multiplication e ) {
		'''( «e.left.expressionToCode» * «e.right.expressionToCode» )'''
	}
	
	def dispatch CharSequence expressionToCode( Division e ) {
		'''( «e.left.expressionToCode» / «e.right.expressionToCode» )'''
	}

	def dispatch CharSequence expressionToCode( Modulo e ) {
		'''( «e.left.expressionToCode» % «e.right.expressionToCode» )'''
	}

	def dispatch CharSequence expressionToCode( IfThenElseExpression e ) {
		'''( «e.guard.expressionToCode» ? «e.thenBranch.expressionToCode» : «e.elseBranch.expressionToCode» )'''
	}

	def dispatch CharSequence expressionToCode( Not e ) {
		'''( «e.expression.expressionToCode» )'''
	}

	def dispatch CharSequence expressionToCode( UnaryPlus e ) {
		'''( +«e.expression.expressionToCode» )'''
	}

	def dispatch CharSequence expressionToCode( UnaryMinus e ) {
		'''( -«e.expression.expressionToCode» )'''
	}

	def dispatch CharSequence expressionToCode( FieldAccess e ) {
		'''( «e.source.expressionToCode».«e.field.name» )'''
	}

	def dispatch CharSequence expressionToCode( NodeExpressionOrArrayAccess e ) {
		''' «e.source.expressionToCode»[«FOR v:e.values SEPARATOR ","» «v.expressionToCode» «ENDFOR»] '''
	}

	def dispatch CharSequence expressionToCode( PreSetExpression e ) {
		'''( «e.source.expressionToCode».pre )'''
	}

	def dispatch CharSequence expressionToCode( PoSetExpression e ) {
		'''( «e.source.expressionToCode».post )'''
	}

	def dispatch CharSequence expressionToCode( Reference e ) {
		'''«e.reference.name»«IF e.isIsCall»(«FOR v:e.args SEPARATOR ','»«v.expressionToCode»«ENDFOR»)«ENDIF»'''
	}

	def dispatch CharSequence expressionToCode( SetExpression e ) {
		'''{:«FOR v:e.values SEPARATOR ','» «v.expressionToCode» «ENDFOR»:}'''
	}
	
	def dispatch CharSequence expressionToCode( ListExpression e ) {
		'''[:«FOR v:e.values SEPARATOR ','» «v.expressionToCode» «ENDFOR»:]'''
	}
	
	def dispatch CharSequence expressionToCode( LambdaParameter e ) {
		'''*'''
	}
	
	def dispatch CharSequence expressionToCode( Locations e ) {
		'''locations'''
	}
	
	def dispatch CharSequence expressionToCode( MyLocation e ) {
		'''loc'''
	}
	
	def dispatch CharSequence expressionToCode( AtomicTrue e ) {
		'''true'''
	}
	
	def dispatch CharSequence expressionToCode( AtomicFalse e ) {
		'''false'''
	}
	
	def dispatch CharSequence expressionToCode( AtomicInteger e ) {
		'''«e.value»'''
	}
	
	def dispatch CharSequence expressionToCode( AtomicReal e ) {
		//'''«e.value.prettyPrint»'''
		String.format("%g",e.value)
	}
	
	def dispatch CharSequence expressionToCode( AtomicRecord e ) {
		'''[ «FOR f:e.fields SEPARATOR ','» «f.field.name» = «f.value.expressionToCode» «ENDFOR»]'''
	}
	
	def CharSequence fieldAssignmentToCode( FieldAssignment f ) {
		'''«f.field.name» = «f.value.expressionToCode»'''
	} 
	
	def dispatch CharSequence expressionToCode( AtomicNow f ) {
		'''now'''
	} 
	
	def dispatch CharSequence expressionToCode( None f ) {
		'''none'''
	} 
	
	def dispatch CharSequence expressionToCode( SetComp f ) {
		'''#{ «f.variable.componentComprehensionToCode» | «f.predicate.expressionToCode» }'''
	} 
	
	def componentComprehensionToCode( ComponentComprehension c ) {
		switch c {
			AllComponents: '''*'''
			AComponentAllStates: '''«c.comp.name»[ * ]'''
			AComponentAState: '''«c.comp.name»[ «c.state.processCompositionToCode» ]'''
		}	
	}
	
	def dispatch CharSequence processCompositionToCode( ParallelComposition p ) {
		'''«p.left.processCompositionToCode» | «p.right.processCompositionToCode»'''
	}
	
	def dispatch CharSequence processCompositionToCode( ProcessReference p ) {
		'''«p.expression.name»'''
	}

	def dispatch CharSequence expressionToCode( AtomicPi e ) {
		'''PI'''
	} 
	
	def dispatch CharSequence expressionToCode( AtomicExp e ) {
		'''E'''
	} 
	
	def dispatch CharSequence expressionToCode( NormalSampling e ) {
		'''NORMAL( «e.mean.expressionToCode» , «e.sd.expressionToCode» )'''
	} 

	def dispatch CharSequence expressionToCode(AbsFunction e) {
		'''abs( «e.arg.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(AcosFunction e) {
		'''acos( «e.arg.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(AsinFunction e) {
		'''asin( «e.arg.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(AtanFunction e) {
		'''atan( «e.arg.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(Atan2Function e) {
		'''atan2( «e.first.expressionToCode» , «e.second.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(CbrtFunction e) {
		'''cbrt( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(CeilFunction e) {
		'''ceil( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(CosFunction e) {
		'''cos( «e.arg.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(ExpFunction e) {
		'''exp( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(FloorFunction e) {
		'''floor( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(LogFunction e) {
		'''log( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(Log10Function e) {
		'''log10( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(MaxFunction e) {
		'''max( «e.first.expressionToCode» , «e.second.expressionToCode» )'''
	} 

	def dispatch CharSequence expressionToCode(MaxMeasure e) {
		'''max{ «e.value.expressionToCode»«IF e.guard != null» | «e.guard.expressionToCode»«ENDIF» }'''
	} 

	def dispatch CharSequence expressionToCode(MinFunction e) {
		'''min( «e.first.expressionToCode» , «e.second.expressionToCode» )'''
	} 

	def dispatch CharSequence expressionToCode(MinMeasure e) {
		'''min{ «e.value.expressionToCode»«IF e.guard != null» | «e.guard.expressionToCode»«ENDIF» }'''
	} 

	def dispatch CharSequence expressionToCode(AverageMeasure e) {
		'''avg{ «e.value.expressionToCode»«IF e.guard != null» | «e.guard.expressionToCode»«ENDIF» }'''
	} 

	def dispatch CharSequence expressionToCode(PowFunction e) {
		'''pow( «e.first.expressionToCode» , «e.second.expressionToCode» )'''
	} 

	def dispatch CharSequence expressionToCode(SinFunction e) {
		'''sin( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(SqrtFunction e) {
		'''sqrt( «e.arg.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(TanFunction e) {
		'''tan( «e.arg.expressionToCode» )'''
	} 
				
	def dispatch CharSequence expressionToCode(PreFunction e) {
		'''pre( «e.arg.expressionToCode» )'''
	} 
				
	def dispatch CharSequence expressionToCode(PostFunction e) {
		'''post( «e.arg.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(UniformFunction e) {
		if (e.args.length > 1) {
			'''U( «FOR v:e.args SEPARATOR ','»«v.expressionToCode»«ENDFOR» )'''
		} else {
			if (e.args.length > 0) {
				'''U( «e.args.get(0).expressionToCode» )'''
			} else {
				''''''
			}
		}
	} 
	
	def dispatch CharSequence expressionToCode(AccessToEdgeValue e) {
		'''edgeValues( «e.src.expressionToCode» , «e.label.name» , «e.trg.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(NewListFunction e) {
		'''newList( «e.arg1.typeToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(NewSetFunction e) {
		'''newSet( «e.arg2.typeToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(SizeFunction e) {
		'''size( «e.arg1.expressionToCode» )'''
	} 
		
	def dispatch CharSequence expressionToCode(MapFunction e) {
		'''map( «e.arg1.expressionToCode» , «e.arg2.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(FilterFunction e) {
		'''filter( «e.arg1.expressionToCode» , «e.arg2.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(ExistsFunction e) {
		'''filter( «e.arg1.expressionToCode» , «e.arg2.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(ForAllFunction e) {
		'''forall( «e.arg1.expressionToCode» , «e.arg2.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(SelectFunction e) {
		'''select( «e.arg1.expressionToCode» , «e.arg2.expressionToCode» )'''
	} 
	
	def dispatch CharSequence expressionToCode(MyContext e) {
		'''«e.attributeTargetToCode»'''
	} 

	def dispatch CharSequence expressionToCode(GlobalContext e) {
		'''«e.attributeTargetToCode»'''
	} 
	
	def dispatch CharSequence expressionToCode(ReceiverContext e) {
		'''receiver.«e.attribute.attributeTargetToCode»'''
	} 
	
	def dispatch CharSequence expressionToCode(SenderContext e) {
		'''sender.«e.attribute.attributeTargetToCode»'''
	} 
	
	def dispatch CharSequence expressionToCode(CastToInteger e) {
		'''int( «e.arg.expressionToCode» )'''
	} 

	def dispatch CharSequence expressionToCode(CastToReal e) {
		'''real( «e.arg.expressionToCode» )'''
	} 

	def dispatch CharSequence expressionToCode(TupleExpression e) {
		'''[ «FOR v:e.values SEPARATOR ','» «v.expressionToCode» «ENDFOR» ]'''
	} 

	def dispatch CharSequence expressionToCode(AtomicRnd e) {
		'''RND'''
	}
	
	def dispatch CharSequence expressionToCode(Range e) {
		'''«e.min.expressionToCode» : «e.max.expressionToCode»'''
	}
	
	def dispatch CharSequence nodeBodyToCode(NamedNode n) {
		'''«n.name» [ «FOR v:n.values SEPARATOR ','» «v.expressionToCode» «ENDFOR» ];'''
	}
	
	def dispatch CharSequence nodeBodyToCode(UnNamedNode n) {
		'''[ «FOR v:n.values SEPARATOR ','» «v.expressionToCode» «ENDFOR» ];'''
	}
	
	def dispatch CharSequence nodeBodyToCode(NodeIfThenElseCommand n) {
		'''
		if ( «n.condition.expressionToCode» )
			«n.thenBlock.nodeBodyToCode»
		«IF n.elseBlock != null»else
			«n.elseBlock.nodeBodyToCode»
		«ENDIF»
		'''
	}
	
	def dispatch CharSequence nodeBodyToCode(NodeBlockCommand n) {
		'''
		{
		«FOR c : n.nodes»
		«c.nodeBodyToCode»
		«ENDFOR»
		}
		'''
	}
	
	def dispatch CharSequence nodeBodyToCode(NodeForCommand n) {
		'''
		for «n.variable.name» from «n.start.expressionToCode»«IF n.step != null» by «n.step.expressionToCode»«ENDIF» to «n.end.expressionToCode»
		 	«n.body.nodeBodyToCode»
		'''
	}
	
	def dispatch CharSequence nodeBodyToCode(NodeForEach n) {
		'''
		for «n.iteration»
			«n.body.nodeBodyToCode»
		'''
	}
	
	def dispatch CharSequence connectionBodyToCode(ConnectionDeclaration c) {
		val hasProperties = (c.edgeProperties != null) && !c.edgeProperties.empty
		'''
		«c.source.locationExpressionToCode» «c.direction.directionToCode» «c.target.locationExpressionToCode»«IF hasProperties» { «FOR p : c.edgeProperties SEPARATOR ', '»«p.edgePropertyToCode»«ENDFOR» }«ENDIF»;
		'''
	}
	
	def dispatch CharSequence connectionBodyToCode(ConnectionIfThenElseCommand c) {
		'''
		if ( «c.condition.expressionToCode» )
			«c.thenBlock.connectionBodyToCode»
		«IF c.elseBlock != null»else
			«c.elseBlock.connectionBodyToCode»
		«ENDIF»
		'''
	}
	
	def dispatch CharSequence connectionBodyToCode(ConnectionBlockCommand c) {
		'''
		{
		«FOR e : c.edges»
		«e.connectionBodyToCode»
		«ENDFOR»
		}
		'''
	}
	
	def dispatch CharSequence connectionBodyToCode(ConnectionForCommand c) {
		'''
		for «c.variable.name» from «c.start.expressionToCode»«IF c.step != null» by «c.step.expressionToCode»«ENDIF» to «c.end.expressionToCode»
		 	«c.body.connectionBodyToCode»
		'''
	}
	
	def dispatch CharSequence connectionBodyToCode(ConnectionForEach c) {
		'''
		for «c.iteration»
			«c.body.connectionBodyToCode»
		'''
	}
	
	def dispatch CharSequence locationExpressionToCode(NamedLocationExpression e) {
		'''«e.ref»[ «FOR v : e.values SEPARATOR ', '»«v.expressionToCode»«ENDFOR» ]'''
	}
	
	def dispatch CharSequence locationExpressionToCode(UnNamedLocationExpression e) {
		'''[ «FOR v : e.values SEPARATOR ', '»«v.expressionToCode»«ENDFOR» ]'''
	}
	
	def CharSequence directionToCode(Direction d) {
		switch d {
			DirectedEdge: '->'
			UnDirectedEdge:'<->'
		}
	}
	
	def CharSequence edgePropertyToCode(EdgeProperty p) {
		'''«p.name» = «p.value.expressionToCode»'''
	}
	
	def dispatch CharSequence areaBodyToCode(AreaElementDeclaration a) {
		'''
		«a.node.locationExpressionToCode» ;
		'''
	}
	
	def dispatch CharSequence areaBodyToCode(AreaIfThenElseCommand a) {
		'''
		if ( «a.condition.expressionToCode» )
			«a.thenBlock.areaBodyToCode»
		«IF a.elseBlock != null»else
			«a.elseBlock.areaBodyToCode»
		«ENDIF»
		'''
	}
	
	def dispatch CharSequence areaBodyToCode(AreaBlockCommand a) {
		'''
		{
		«FOR n : a.nodes»
		«n.areaBodyToCode»
		«ENDFOR»
		}
		'''
	}
	
	def dispatch CharSequence areaBodyToCode(AreaForCommand a) {
		'''
		for «a.variable.name» from «a.start.expressionToCode»«IF a.step != null» by «a.step.expressionToCode»«ENDIF» to «a.end.expressionToCode»
		 	«a.body.areaBodyToCode»
		'''
	}
	
	def dispatch CharSequence areaBodyToCode(AreaForEach a) {
		'''
		for «a.iteration»
			«a.body.areaBodyToCode»
		'''
	}
	
	
	
	//TODO Are the limits available somewhere as constants? (instead of taken
	// from the docs, as here)
	//TODO This looks ugly, especially the special handling of 0. Can it be
	// improved? (flag # looks like it should force decimal separator but it
	// doesn't seem to work). DecimalFormat can set a minimum number of fractional
	// digits but it seems like overkill for the common case. 
//	def CharSequence prettyPrint(double d) {
//		// Remember that these values will only be positive, as negative literals
//		// are handled via UnaryMinus
////		if (d == 0) {
////			"0.0"
////		}
////		else if (d >= 1e-3 && d < 1e7) {
//////			String.format("%#f",d)
////			String.valueOf(d)
////		} else {
////			new BigDecimal(d).toPlainString
////		}
//		String.format("%g",d)
//	}

}