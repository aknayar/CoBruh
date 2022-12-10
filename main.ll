
; ModuleID = 'CoBruh'
source_filename = "CoBruh"

@fmt = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare void @printf(i8*, ...)

define void @main() {
entry:
  %a = alloca double, align 8
  store double 5.000000e+00, double* %a, align 8
  %i = alloca double, align 8
  store double 0.000000e+00, double* %i, align 8
  br label %loop

loop:                                             ; preds = %loop_block, %entry
  %i3 = load double, double* %i, align 8
  %a4 = load double, double* %a, align 8
  %tmp5 = fcmp olt double %i3, %a4
  br i1 %tmp5, label %loop_block, label %merge

loop_block:                                       ; preds = %loop
  %i1 = load double, double* %i, align 8
  call void (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), double %i1)
  %i2 = load double, double* %i, align 8
  %tmp = fadd double %i2, 5.000000e-01
  store double %tmp, double* %i, align 8
  br label %loop

merge:                                            ; preds = %loop
  ret void
}

