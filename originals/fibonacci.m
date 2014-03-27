lvalue n
push 100
:=
lvalue f0
push 0 
:=
rvalue f0
print 
lvalue f1
push 1
:=
rvalue f1
print
label LABL1
rvalue n 
gofalse LABL2
lvalue fnew 
rvalue f0
rvalue f1
+
:=
rvalue fnew 
print 
lvalue f0
rvalue f1
:=
lvalue f1 
rvalue fnew 
:=
lvalue n
rvalue n
push 1
-
:=
goto LABL1
label LABL2
halt  