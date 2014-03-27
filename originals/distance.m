lvalue x1
push 0
:= 
lvalue y1
push 0
:=
lvalue x2
push 1
:=
lvalue y2
push 0
:=
lvalue S
rvalue x1
rvalue x2 
-
rvalue x1
rvalue x2
- 
*
rvalue y1
rvalue y2 
-
rvalue y1
rvalue y2
- 
*
+
:=
lvalue d
rvalue S
:=
lvalue n
push 50
:=
label LABL1
rvalue n
gofalse LABL2
lvalue d
rvalue d
rvalue S
rvalue d
/
+
push 2
/
:=
lvalue n
rvalue n
push 1
-
:=
goto LABL1
label LABL2
rvalue d
print 
halt