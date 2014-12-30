//
// semantics
//
// semantical anaysis
// - constants assignments (range, type, unary operators)
// - variable definitions (use before def, multiple definitions)
// - function parameters (number, type)
// - type checking
//   - expressions: compatible operations/operands
//   - assignments: type of LHS = type of RHS
//   - return statements: correct type
//   - boolean types for conditions

module semantics;


var u,v: boolean;
    x,y: integer;


// nothing wrong here, just a helper procedure
procedure ProcedureCall();
begin
end ProcedureCall;
// constant assignments and unary +/-/! operations
procedure Constants;
var a,b: boolean;
    i,j: integer;
begin
  a := true;                // pass
  a := false;               // pass
  a := !true;               // pass
  a := !!!true;             // pass
  a := !false;              // pass
	a := true;                // modified, pass (unaryop: type mismatch.)
	a := !!!false;            // modified, pass (unaryop: type mismatch.)

  i := -0;                  // pass
  i := +0;                  // pass
	i := -0;                  // modified, pass (factor expected.)
	i := +0;                  // modified, pass (factor expected.)
  i := -2147483648;         // pass (min int)
  j :=  2147483647;         // pass (max int)
	i := -2147483648-1;       // modified, pass (integer constant outside valid range.) 
  j :=  2147483647+2;       // modified, pass (integer constant outside valid range.)

  a := true && false || true && !!!!!!!!!!false	 // modified, pass (incompatible types in assignment.)
end Constants;


// variable definitions
procedure UseBeforeDef(k);
var i,j: integer;
begin
  i := j;                   // pass
  i := k;                   // pass
  i := x;                   // pass
  i := x+y;                 // modified, pass (unidentified identifier 'z')

  i := 0
end UseBeforeDef;


// multiple definitions
procedure MultipleDef(l,j,k);	// modified, pass (duplicate parameter declaration 'i')
var i: integer;								// modified, pass (duplicate variable declaration 'i')
    m: integer;								// modified, pass (duplicate variable declaration 'l')
begin
  i := 1
end MultipleDef;


// parameters: too many/few/wrong type of parameters
// each line except 'Parameters(1,2);' contains an error
procedure Parameters(p1, p2);
begin
  Parameters(p2, p2);       // modified, pass (not enough arguments.)
  Parameters(1, 1);         // modified, pass (not enough arguments.)
  Parameters(1+2, 3+4);		  // modified, pass (argument type mismatch.)
  Parameters(1,2);	        // modified, pass (too many arguments.)
  Parameters(1,99999);      // modified, pass (too many arguments.)

  Parameters(p2,p1);        // pass
  Parameters(1,2)           // pass
end Parameters;


// type checks
procedure Expressions(p1, p2, p3, p4);
var a,b,c: boolean;
    i,j,k: integer;
begin
  a := false && true;       // modified, pass (binop: type mismatch.)
	a := true # false;        // modified, pass (binop: type mismatch.)
	a := b || !!!c;	          // modified, pass (binop: type mismatch.)
	a := i > j;               // modified, pass (binop: type mismatch.)
  a := !!!b;                // pass
  a := a && (!b);           // pass

	i := j + k;               // modified, pass (binop: type mismatch.)
	i := j / k * p1;          // modified, pass (binop: type mismatch.)
	i := -j;                  // modified, pass (binop: type mismatch.)
  i := j + k;               // modified, pass (factor expected.)
  i := j + (-k);            // pass

  a := a && !b && (i < j)   // pass
         && (j < k)
         || (i = k)
end Expressions;


// assignment type checks
procedure Assignments(p1, p2, p3, p4);
var a,b,c: boolean;
    i,j,k: integer;
begin
  a := i # j;	              // modified, pass (incompatible types in assignment.)
  a := a;                   // modified, pass (incompatible types in assignment.)
  i := p1+p2+p3+p4+i+j+k    // modified, pass (incompatible types in assignment.)
end Assignments;


// return statements type checks
procedure ProcReturn();
begin
					                  // superfluous expression after return.
end ProcReturn;

function NoReturn(): integer;
begin
  return 1+2-3*4/5+(6-7/8*9)	// modified, pass (expression expected after return.) 
end NoReturn;									// modified, pass (procedure/function identifier mismatch ('NoReturn' != 'IntReturn').)

function IntReturn(): integer;
begin
  return 1 + 2              // modified, pass (return type mismatch.)
end IntReturn;

function BoolReturn(): boolean;
begin
  return 1 > 2              // modified, pass (return type mismatch.)
end BoolReturn;


// condition type checking
procedure If(p1, p2);
begin
  if ((p1+ p2+p2>p2) && true) then         // modified, pass (boolean expression expected.)
    return
  else
    return
  end;

  if (BoolReturn()) then		// modified, pass (boolean expression expected.)
    return
  else
    return
  end
end If;

procedure While();
var a,b: integer;
begin
  while (BoolReturn() # true) do              // modified, pass (boolean expression expected.)
    b := b-1
  end
end While;

begin
  if (1+2+3>4+5) then x:=1+2 else x:=4+5 end
end semantics.
