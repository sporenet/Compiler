module ir;


var u,v: boolean;
    x,y: integer;

procedure ProcedureCall();
begin
end ProcedureCall;

procedure BoolConst;
var a,b,c,d: boolean;
    a01,a02,a03,a04,a05,
		a06,a07,a08,a09,a10,
		a11,a12,a13,a14,a15,
		a16,a17,a18,a19,a20,
		a21,a22,a23,a24: boolean;

begin
	a01 := true && false;												// assign false directly
	a02 := true && true;												// assign true directly
	a03 := false && false;											// assign false directly
	a04 := true && b;														// assign based on b
	a05 := false && b;													// assign false directly
	a06 := b && c;															// assign based on b and c
	a07 := (b && true) && (c && false);					// complicated, but false
	a08 := (b && false) && (c && false);				// complicated, but false
	a09 := (b && true) && (c && true);					// complicated, based on b and c
	a10 := b && (c && d);												// complicated, based on b, c and d
	a11 := true || true;												// assign true directly
	a12 := false || true;												// assign true directly
	a13 := false || false;											// assign false directly
	a14 := true || b;														// assign true directly
	a15 := b || false;													// assign based on b
	a16 := b || c;															// assign based on b and c
	a17 := (b || true) || (c || d);							// complicated, but true
	a18 := (b || c) || (false || d);						// complicated, based on b, c and d
	a19 := (b || c) || d;												// complicated, based on b, c and d
	a20 := (b && c) || d;												// true if b and c are true or d is true
	a21 := (b && true) || d;										// true if b is true or d is true
	a22 := (b && false) || d;										// true if d is true
	a23 := (false && true) || (true || false);	// always true
	a24 := (b || true) && (c && d)							// true if c and d are true
end BoolConst;

procedure BoolExpr;
var a,b,c,d: boolean;
		x,y,z,w: integer;
		a01,a02,a03,a04,a05,
		a06,a07,a08,a09,a10: boolean;

begin
	a01 := x + 3 > y;
	a02 := a # (b && true);
	a03 := true # (false && true);
	a04 := (x = y) || (true && (3 - 4 = 1));
	a05 := true # false || b && (x > 3);
	a06 := 1 + 2 + 3 + x + y + 4 > 5;
	a07 := x * y / 0 * z # y * 3;
	a08 := true # (x > y + 4 / 0);
	a09 := (1>2) && true;
	a10 := true # (0 = 0)
end BoolExpr;

procedure IntExpr;
var a0,a1,a2,a3: integer;

begin
	a0 := 1+2*(3/4+a0/a1);
	a1 := a0+a1+a2+a3+0+1+2+3+4/5/6/7/8/9*100000;
	a2 := -(((((((((1-2)*3)/4)+5)/6)*7)/8)-9)+10)-33;
	a3 := -1-2-3-4-5*(6+7/8*9)
end IntExpr;

procedure If;
var a,b,c: integer;
		t,f: boolean;

begin
	if (true) then a:=1 else b:=1 end;
	if (false) then a:=3 else b:=4 end;
	if (true && false) then a:=5 else b:=6 end;
	if ((t&&f)||(true||false)) then a:=7 else b:=8 end;
	if ((t&&f)&&true) then a:=9 else b:=10 end;
	if (true&&(false||t)) then a:=11 else b:=12 end;
	if ((1>2)&&true) then a:=12 else b:=13 end;
	if (true#(false&&true)) then if (true#false||t&&(x>3)) then a:=b+c else b:=1 end; a:=1 else b:=999 end
end If;

procedure While;
var a,b,c: integer;
		t,f: boolean;

begin
	while (true) do a:=1; b:=1 end;
	while (false) do a:=2; b:=2 end;
	while (true || false) do a:=3; b:=3 end;
	while ((true&&false)||(t&&f)) do a:=4; b:=4 end;
	while (t||(f||(true&&false))) do a:=5; b:=5 end;
	while (t&&(t||f)) do a:=6; b:=6 end;
	while (true||(t&&f)) do while ((a = 7) || (b = 7 + a)) do if (true&&f) then a:=7 else b:=7 end; a:=8 end; a:=9 end
end While;

function Parameters(a,b,c,d,e,f,g): integer;
var x,y: integer;
begin
	x:=Parameters(1+x,2+y,3+x+y,a,b,c,d);
	y:=1+2+Parameters(1,2,3,4,5,6,7)+Parameters(0,0,x*y,x*0,3,3,x+a+b);
	return a+b+c+d+3+x
end Parameters;

function BoolReturn(a,b): boolean;
begin
 return a>b
end BoolReturn;

procedure foo;
var a,b:integer;
		c,d:boolean;
begin
	if (BoolReturn(1,2)&&(c&&false)) then a:=1 end;
	if (c||(d&&BoolReturn(3,4))) then c:=BoolReturn(2,2)||false else c:=true||BoolReturn(2,2) end;
	if (BoolReturn(a,b) && (Parameters(1,2,3,4,5,6,7)>1)) then c:=Parameters(1,2,3,4,5,6,7)#a end
end foo;
begin
end ir.
