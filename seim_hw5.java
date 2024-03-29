/* Name: Edward Seim

   UID: 930713935

   Others With Whom I Discussed Things:
   Joshua Kuroda
   Lauren Konchan

   Other Resources I Consulted:
   
*/

// import lists and other data structures from the Java standard library
// DO NOT IMPORT ANYTHING ELSE
import java.util.*;

/* Java style rules:
 * 1. Do not use type casts or the instanceof operator. You should always use
 *    dynamic dispatch instead.
 * 2. Never use null!
 * 3. Program to the interface, not the implementation. The type of every variable
 *    should be the name of an *interface*, not a *class*.
 */

/* Tips:
 * - You may use any helper methods or classes you need.
 * - You will need to use several classes from the java.util package.
 *   the Java API Reference will be useful:
 *     http://docs.oracle.com/javase/8/docs/api
 * - Write comments to let me know what you're thinking. This will help me 
 *   give you partial credit even if your code doesn't work.
 * - Write more tests!
 */

/* Now for the assignment itself:
   You will be implementing two interpreters for a small calculator language. 
   Here's the grammar of the language:

   (Expressions) E ::= N | E + E | E - E | E * E | E / E
   (Numbers)     N ::= Real numbers

   The Java representation of expressions consists of an interface AExp
   and two classes that implementation the interface: Num represents a single
   number (of Java type Double), and BinOp represents a binary arithmetic operation,
   whose operands are arbitrary arithmetic expressions.

   The enum type Op defines the four binary arithmetic operations. I've done this 
   for you already, since we haven't talked about enums in class. An enum is like
   a regular class, that has only a finite number of instances (objects of that 
   class) -- in this case, Op.PLUS, Op.MINUS, Op.TIMES, and Op.DIVIDE. You can read
   more Java's enums at:
     https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

   Problem 1:
    The first interpreter will be similar to the one from homework 3: It will
    directly evaluate the Java representation of arithmetic expressions to their
    numeric value. First, add constructors to the Num and BinOp classes. Now
    you can build representations of expressions. For example, the representation
    of (1.0 + 2.0) * 4.0 can be constructed by the expression:
     new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)), Op.TIMES, new Num(4.0))

    Next, uncomment the eval() method of AExp, and add implementations to Num and BinOp. 
    Use the Op.calculate() methods I've provided to help evaluate BinOps.

    Uncomment the first test case in CalcTest.main for an example. Write more tests!
    Before moving to Problem 2, make sure to test your code by running:

    $ javac hw5.java
    $ java CalcTest
*/

/* Problem 2:
   Java programs are compiled to a bytecode language that consists of
   a sequence of instructions for a stack machine.  A stack machine
   evaluates instructions using only a stack to record the values of
   subexpressions as it computes, as opposed to storing values in
   variables (e.g. in an environment) or machine registers.  This style
   makes the code compact and also easy to compile to machine code.
   For example, the arithmetic expression (1.0 + 2.0) * 4.0 can be
   expressed as the sequence of instructions:

    PUSH 1.0
    PUSH 2.0
    PLUS
    PUSH 4.0
    TIMES

    To illustrate execution by showing the state of the stack after
    each step of computation. Initially, the stack is empty:

      Code            Stack
    --------------+------------------
    PUSH 1.0      |
    PUSH 2.0      |   
    PLUS          |   
    PUSH 4.0      |   -------
    TIMES         |   

    After executing PUSH 1.0, the stack contains a single value 1.0.
    The asterisk indicates where we are in the execution of the code.
    
      Code            Stack
    --------------+------------------
  * PUSH 1.0      |
    PUSH 2.0      |   
    PLUS          |    1.0
    PUSH 4.0      |   -------
    TIMES         |   

    After executing PUSH 2.0, the stack contains two values: 2.0 at
    the top, and 1.0 below it.
    
      Code            Stack
    --------------+------------------
    PUSH 1.0      |
  * PUSH 2.0      |    2.0
    PLUS          |    1.0
    PUSH 4.0      |   -------
    TIMES         |   

    The PLUS operation pops the top two values off the stack, and
    then pushes their sum back onto the stack:
    
      Code            Stack
    --------------+------------------
    PUSH 1.0      |
    PUSH 2.0      |    
  * PLUS          |    3.0
    PUSH 4.0      |   -------
    TIMES         |   

    Next we PUSH 4.0 onto the top of the stack:
    
      Code            Stack
    --------------+------------------
    PUSH 1.0      |
    PUSH 2.0      |    4.0
    PLUS          |    3.0
  * PUSH 4.0      |   -------
    TIMES         |   

    Finally, TIMES pops off the top two values, and pushes
    back their product:
    
      Code            Stack
    --------------+------------------
    PUSH 1.0      |
    PUSH 2.0      |    
    PLUS          |    12.0
    PUSH 4.0      |   -------
  * TIMES         |   
    
    Now execution is finished, the resulting value is on the top
    of the stack.

  In this problem we will define our own stack-machine-based
  representation of the arithmetic expressions from the prior problem.
  An arithmetic computation is now expressed as a sequence (i.e., a
  list) of instructions. The grammar of instructions is:

  (Instruction) I ::= PUSH N | PLUS | MINUS | TIMES | DIVIDE

  Note that unlike expressions above, instructions are not recursive:
  No instruction contains other instructions. The Java representation
  of instructions comprises an interface AInstr and two classes Push
  and Calculate that implement the interface. Like the BinOp class for
  expressions, the Calculate expression contains an Op field.

  A Push instruction pushes a new number (a Double) onto the stack.  A
  Calculate instruction with operation op pops the top two numbers n1
  and n2 off the stack and pushes the result of evaluating (n2 op n1)
  onto the stack (thereby decreasing the stack size by 1).  Note that
  the first operand in the computation is the second value popped off
  the stack, and the second operand is the first value popped off the
  stack.  This makes a difference for non-commutative operations like
  subtraction and division.  This behavior makes sense since it
  corresponds with the order in which the operands were originally
  computed (and pushed onto the stack). Make sure to test the
  non-commutative operations!

  Implement a constructor for the Push and Calculate classes.  For
  example, the arithmetic expression (1.0 + 2.0) * 4.0 would be
  represented by this sequence of instructions: 
    new Push(1.0)
    new Push(2.0)
    new Calculate(Op.PLUS)
    new Push(3.0)
    new Calculate(Op.TIMES)

  Uncomment the AInstr.eval() method and add implementations for Push
  and Calculate. AInstr.eval() evaluates an instruction in the context
  of a given stack (modifying the stack). The result of evaluating an
  instruction should always be at the top of the stack when eval()
  returns.

  The class Instrs represents a list of AInstr instructions. Uncomment
  its eval method and implement it. Use Java's for-each loop to
  iterate over the list of instructions:
    http://docs.oracle.com/javase/1.5.0/docs/guide/language/foreach.html

  Once all of the stack operations have been processed, the number at
  the top of the stack will be the final result value to return.

  Uncomment the test for Problem 2 in CalcTest.main(). Compile and run,
  and add more tests!
 */

/* Problem 3:

   Uncomment the method compile in AExp and implement it; this method
   compiles (i.e., translates) an arithmetic expression into a list of
   arithmetic instructions that represents the same computation.  For
   example, the representation of (1.0 + 2.0) * 4.0 shown in Problem 1
   above should be compiled to the list of instructions shown in
   Problem 2 above.  There should be a Calculate instruction in the
   output list for every BinOp in the input; evaluating the input
   expression to a number n and then returning a list of one
   instruction of the form new Push(n) will get you no credit.
   
   Hint: This function corresponds exactly to a postorder traversal of
   the input expression when viewed as a tree.

   Uncomment the third test case in CalcTest.  Define a toString()
   method in each class implementing AInstr for testing purposes.  For
   instance, a reasonable output of the given test case is [Push 1.0,
   Push 2.0, Calculate PLUS, Push 4.0, Calculate TIMES].  (In Java,
   the + operator for string concatenation is highly overloaded, and
   in particular an invocation ("hi" + o) for any object o is
   equivalent to "hi" + o.toString().  Java lists already have a
   well-defined toString() method that recursively converts each
   element to a string.)
 */
     
// a type for arithmetic expressions
interface AExp {
    Double eval(); 	                       // Problem 1
    List<AInstr> compile(); 	               // Problem 3
}

class Num implements AExp {
    protected Double val;

    public Num (Double val) {
      this.val = val;
    }

    public Double eval () {
        return this.val;
    }

    public List<AInstr> compile () {
        List<AInstr> result = new LinkedList<AInstr>();
        result.add(new Push(this.val));
        return result;
    }
}

class BinOp implements AExp {
    protected AExp left, right;
    protected Op op;

    public BinOp (AExp left, Op op,  AExp right) {
        this.left = left;
        this.op = op;
        this.right = right;
    }

    public Double eval () {
        return this.op.calculate(this.left.eval(), this.right.eval());
    }

    public List<AInstr> compile () {
        List<AInstr> result = new LinkedList<AInstr>();
        result.addAll(left.compile());
        result.addAll(right.compile());
        result.add(new Calculate(this.op));
        return result;
    }
}

// a representation of four arithmetic operators
enum Op {
    PLUS { public Double calculate(Double a1, Double a2) { return a1 + a2; } },
    MINUS { public Double calculate(Double a1, Double a2) { return a1 - a2; } },
    TIMES { public Double calculate(Double a1, Double a2) { return a1 * a2; } },
    DIVIDE { public Double calculate(Double a1, Double a2) { return a1 / a2; } };

    abstract Double calculate(Double a1, Double a2);
}

// a type for arithmetic instructions
interface AInstr {
    void eval(Stack<Double> stack);    // Problem 2
}

class Push implements AInstr {
    protected Double val;

    public Push (Double val) {
        this.val = val;
    }

    public void eval (Stack<Double> stack) {
        stack.push(this.val);
    }

    public String toString () {
        return "Push " + this.val;
    }
}

class Calculate implements AInstr {
    protected Op op;

    public Calculate (Op op) {
        this.op = op;
    }

    public void eval (Stack<Double> stack) {
        Double val2 = stack.pop();
        Double val1 = stack.pop();
        stack.push(op.calculate(val1, val2));
    }

    public String toString () {
        return "Calculate " + this.op;
    }
}

class Instrs {
    protected List<AInstr> instrs;

    public Instrs(List<AInstr> instrs) { this.instrs = instrs; }

    public Double eval() {
        Stack<Double> stack = new Stack<Double>();
        for (AInstr instr : instrs) {
            instr.eval(stack);
        }
        return stack.pop();
    }  // Problem 2
}


class CalcTest {
    public static void main(String[] args) {
	    // a test for Problem 1
    	AExp aexp =
    	    new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
    		      Op.TIMES,
    		      new Num(4.0));
    	System.out.println("aexp evaluates to " + aexp.eval()); // aexp evaluates to 12.0

        AExp aexp2 =
            new BinOp(new BinOp(new Num(9.0), Op.MINUS, new Num(5.0)),
                  Op.DIVIDE,
                  new Num(2.0));
        System.out.println("aexp2 evaluates to " + aexp2.eval()); // aexp evaluates to 2.0

        // a test for Problem 2
    	List<AInstr> is = new LinkedList<AInstr>();
    	is.add(new Push(1.0));
    	is.add(new Push(2.0));
    	is.add(new Calculate(Op.PLUS));
    	is.add(new Push(4.0));
    	is.add(new Calculate(Op.TIMES));
    	Instrs instrs = new Instrs(is);
    	System.out.println("instrs evaluates to " + instrs.eval());  // instrs evaluates to 12.0

        List<AInstr> sec = new LinkedList<AInstr>();
        sec.add(new Push(2.0));
        sec.add(new Push(1.0));
        sec.add(new Calculate(Op.MINUS));
        sec.add(new Push(4.0));
        sec.add(new Calculate(Op.TIMES));
        Instrs instrsec = new Instrs(sec);
        System.out.println("instrsec evaluates to " + instrsec.eval());  // instrsec evaluates to 4.0

        // a test for Problem 3
        System.out.println("aexp converts to " + aexp.compile());
        System.out.println("aexp2 converts to " + aexp2.compile());

    }
}
