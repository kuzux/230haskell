lvalue a
push 10
:=
lvalue b
push 20
:=
lvalue a
rvalue a
rvalue b
+
:=
lvalue b
rvalue a
rvalue b
-
:=
lvalue a
rvalue a
rvalue b
-
:=
rvalue a
print
rvalue b
print
halt
