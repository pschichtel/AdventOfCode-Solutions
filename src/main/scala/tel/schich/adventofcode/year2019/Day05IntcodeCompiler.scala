package tel.schich.adventofcode.year2019

object Day05IntcodeCompiler {
    sealed trait Expr

    case class IfThisThanThat(cond: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr
    case class LessThan(lhs: Expr, rhs: Expr) extends Expr
    case class Eq(lhs: Expr, rhs: Expr) extends Expr
    case class Add(lhs: Expr, rhs: Expr) extends Expr
    case class Multiply(lhs: Expr, rhs: Expr) extends Expr
    case object ReadInput extends Expr
    case class WriteOutput(expr: Expr) extends Expr
    case class Literal(value: Int) extends Expr
    case class Chain(statements: Seq[Expr]) extends Expr
    case class VariableDefinition(name: String, value: Expr) extends Expr
    case class VariableReference(name: String) extends Expr

    sealed abstract class Instr(size: Int)
    case class Op(opCode: Int, args: Seq[Arg], result: Symbol) extends Instr(args.length + 1)
    case class Val(value: Int, symbol: Symbol) extends Instr(1)
    case class Label(symbol: Symbol) extends Instr(0)

    sealed trait Arg
    case class Value(value: Int) extends Arg
    case class Symbol(id: Int) extends Arg



    val prog = Chain(Seq(VariableDefinition("a", ReadInput), VariableDefinition("b", ReadInput), WriteOutput(IfThisThanThat(ReadInput, Add(VariableReference("a"), VariableReference("b")), Multiply(Multiply(VariableReference("a"), VariableReference("b")), Literal(2))))))
    println(prog)

    type SymbolLookup = Map[String, Symbol]
    type CompilerState = (Seq[Instr], Int, Symbol, SymbolLookup)

    def compileBinaryOp(opCode: Int, left: Expr, right: Expr, symbolCounter: Int, symbolLookup: SymbolLookup): CompilerState = {
        val (leftInstructions, firstAdvancement, leftValue, firstNewSymbolLookup) = compile(left, symbolCounter, symbolLookup)
        val (rightInstructions, secondAdvancement, rightValue, secondNewSymbolLookup) = compile(right, firstAdvancement, firstNewSymbolLookup)
        val output = Symbol(secondAdvancement)
        val plus = Op(opCode, Seq(leftValue, rightValue), output)

        (leftInstructions ++ rightInstructions :+ plus, secondAdvancement + 1, output, secondNewSymbolLookup)
    }

    def compileChain(statements: Seq[Expr], instructions: Seq[Instr], symbolCounter: Int, symbolLookup: SymbolLookup): CompilerState = {
        statements match {
            case head :: tail =>
                val (instr, counter, output, newSymbolLookup) = compile(head, symbolCounter, symbolLookup)
                val newInstructions = instructions ++ instr
                if (tail.isEmpty) (newInstructions, counter, output, newSymbolLookup)
                else compileChain(tail, newInstructions, counter, newSymbolLookup)
            case _ => (Nil, symbolCounter, Symbol(symbolCounter), symbolLookup)
        }
    }

    def compile(expr: Expr, symbolCounter: Int, symbolLookup: SymbolLookup): CompilerState = {
        expr match {
            case Chain(statements) => compileChain(statements, List.empty, symbolCounter, symbolLookup)
            case VariableDefinition(name, value) =>
                val (defInstructions, newCounter, output, newSymbolLookup) = compile(value, symbolCounter, symbolLookup)
                (defInstructions, newCounter, output, newSymbolLookup + (name -> output))
            case VariableReference(name) =>
                (Nil, symbolCounter, symbolLookup(name), symbolLookup)
            case IfThisThanThat(cond, left, right) =>
                val (condInstructions, firstAdvancement, condValue, firstNewSymbolLookup) = compile(cond, symbolCounter, symbolLookup)
                val (trueInstructions, secondAdvancement, leftValue, secondNewSymbolLookup) = compile(left, firstAdvancement, firstNewSymbolLookup)
                val (falseInstructions, thirdAdvancement, rightValue, thirdNewSymbolLookup) = compile(right, secondAdvancement, secondNewSymbolLookup)

                val afterTrueLabel = Symbol(thirdAdvancement)
                val afterFalseLabel = Symbol(thirdAdvancement + 1)
                val output = Symbol(thirdAdvancement + 2)

                val conditionCode = condInstructions ++ Seq(
                    Op(5, Seq(condValue), afterTrueLabel)
                )

                val trueCode = trueInstructions ++ Seq(
                    Op(1, Seq(Value(0), leftValue), output),
                    Label(afterTrueLabel)
                )

                val falseCode = falseInstructions ++ Seq(
                    Op(1, Seq(Value(0), rightValue), output),
                    Label(afterFalseLabel)
                )

                val instr = conditionCode ++ trueCode ++ falseCode

                (instr, thirdAdvancement + 3, output, thirdNewSymbolLookup)
            case LessThan(left, right) => compileBinaryOp(7, left, right, symbolCounter, symbolLookup)
            case Eq(left, right) => compileBinaryOp(8, left, right, symbolCounter, symbolLookup)
            case Add(left, right) => compileBinaryOp(1, left, right, symbolCounter, symbolLookup)
            case Multiply(left, right) => compileBinaryOp(2, left, right, symbolCounter, symbolLookup)
            case ReadInput =>
                val output = Symbol(symbolCounter)
                (Seq(Op(3, Seq(), output)), symbolCounter + 1, output, symbolLookup)
            case WriteOutput(e) =>
                val (instructions, advancedCounter, previousOutput, newSymbolLookup) = compile(e, symbolCounter, symbolLookup)
                (instructions ++ Seq(Op(4, Seq(previousOutput), previousOutput)), advancedCounter + 1, previousOutput, newSymbolLookup)
            case Literal(i) =>
                val symbol = Symbol(symbolCounter)
                (Seq(Val(i, symbol)), symbolCounter + 1, symbol, symbolLookup)
        }
    }

    val (intermediateInstructions, symbolCounter, outputsymbol, finalSymbolLookup) = compile(prog, 0, Map.empty)
    intermediateInstructions.foreach(println)
}
