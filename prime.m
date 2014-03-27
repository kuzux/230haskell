lvalue i
push 100000
:=
lvalue aprev
push 7
:=
lvalue n
push 1
:=
label 0
rvalue i
gofalse 1
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
label 2
rvalue m
gofalse 3
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
goto 2
label 3
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
gofalse 4
rvalue anew
rvalue aprev
-
print
label 4
lvalue aprev
rvalue anew
:=
goto 0
label 1
halt
