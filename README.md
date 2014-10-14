llvm-general-quote
===================
`llvm-general-quote` is a quasiquoting-library for llvm-general.
It aims to support all language constructs of llvm.

In addtion to this, it supports using mutable variables and control structures instead of pure SSA form.
This is translated automatically into SSA through appropriate renaming.


Example:
```
[lldef|
  define i64 @foo(i64 %start, i64 %end) {
    entry:
      %x = i64 0

    for:
      for i64 %i in %start to %end {
          %x = add i64 %i, %x
      }

    exit:
      ret i64 %x
  }
  |]
```
this would be transformed into:
```
define i64 @foo(i64 %start, i64 %end) {
entry:
  br label %for.head

for.head:                      ; preds = %n0, %entry
  %x.12 = phi i64 [ 0, %entry ], [ %x.6, %n0 ]
  %i.4 = phi i64 [ %start, %entry ], [ %i.9, %n0 ]
  %for.cond.3 = icmp slt i64 %i.4, %end
  br i1 %for.cond.3, label %n0, label %for.end

n0:                            ; preds = %for.head
  %x.6 = add i64 %i.4, %x.12
  %i.9 = add nuw nsw i64 %i.4, 1
  br label %for.head

for.end:                       ; preds = %for.head
  ret i64 %x.12
}
```
