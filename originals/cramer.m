lvalue a
push 1
:=
lvalue b
push 2
:=
lvalue c
push 3
:=
lvalue d
push 4
:=
lvalue e
push 1
:=
lvalue f
push 1
:=
lvalue x 
rvalue e 
rvalue d
*
rvalue b
rvalue f
*
-
rvalue a
rvalue d
*
rvalue b
rvalue c
- 
*
/
:=
lvalue y 
rvalue a 
rvalue f
*
rvalue e
rvalue c
*
-
rvalue a
rvalue d
*
rvalue b
rvalue c
- 
*
/
:=
rvalue x
print
rvalue y 
print
halt

