lvalue i
push 100000
:=
lvalue aprev
push 7
:=
lvalue n
push 1
:=
label LABL1
rvalue i
gofalse LABL2
lvalue n
rvalue n
push 1
+
:=
lvalue k
rvalue n
:=
lvalue m
rvalue aprev
:=
label LABL3 
rvalue m 
gofalse LABL4 
lvalue t
rvalue m
:=
lvalue m
rvalue k
rvalue m
mod 
:=
lvalue k
rvalue t
:=
goto LABL3
label LABL4
lvalue anew 
rvalue aprev
rvalue k
+
:=
lvalue i 
rvalue i
push 1
-
:=
rvalue anew
rvalue aprev
-
push 1
-
gofalse LABL5
rvalue anew
rvalue aprev 
-
print
label LABL5
lvalue aprev
rvalue anew 
:=
goto LABL1
label LABL2
halt 

