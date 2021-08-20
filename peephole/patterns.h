/*
 * JOOS is Copyright (C) 1997 Laurie Hendren & Michael I. Schwartzbach
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will the authors be liable for any damages resulting from
 * use of this software.
 *
 * email: hendren@cs.mcgill.ca, mis@brics.dk
 */



/* condition L1
 *   iconst_0
 *   goto L2
 * L1:
 *   iconst_1
 * L2:
 *   dup
 * ifeq L3
 *   pop
 * ------>
 * condition L1
 *   iconst_0
 *   goto L3
 * L1:
 *
 * This is equivalent since in the original code, the result of the first condition
 * dictates the result of "ifeq L3" so we don't need to do all of this extra pushing and popping
 */

int simplify_label_chain_dup_pop1(CODE **c)
{ int l1,l2,l3,l4,l5;
  int v1,v2;

  if (
    uses_label (*c, &l1)            && uniquelabel(l1) &&
    is_ldc_int (nextby(*c, 1), &v1) && v1 == 0 &&
    is_goto    (nextby(*c, 2), &l2) && uniquelabel(l2) &&
    is_label   (nextby(*c, 3), &l3) && l3 == l1 &&
    is_ldc_int (nextby(*c, 4), &v2) && v2 == 1 &&
    is_label   (nextby(*c, 5), &l4) && l4 == l2 &&
    is_dup     (nextby(*c, 6))      &&
    is_ifeq    (nextby(*c, 7), &l5) &&
    is_pop     (nextby(*c, 8))
    ) {
        switch ((*c)->kind){
            case ifeqCK:
                return replace(c, 9,
                        makeCODEifeq(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case ifneCK:
                return replace(c, 9,
                        makeCODEifne(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case ifnullCK:
                return replace(c, 9,
                        makeCODEifnull(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case ifnonnullCK:
                return replace(c, 9,
                        makeCODEifnonnull(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpeqCK:
                return replace(c, 9,
                        makeCODEif_icmpeq(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpneCK:
                return replace(c, 9,
                        makeCODEif_icmpne(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpgtCK:
                return replace(c, 9,
                        makeCODEif_icmpgt(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpltCK:
                return replace(c, 9,
                        makeCODEif_icmplt(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpleCK:
                return replace(c, 9,
                        makeCODEif_icmple(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpgeCK:
                return replace(c, 9,
                        makeCODEif_icmpge(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_acmpeqCK:
                return replace(c, 9,
                        makeCODEif_acmpeq(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_acmpneCK:
                return replace(c, 9,
                        makeCODEif_acmpne(l1,
                        makeCODEldc_int(0,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            default:
                return 0;
        }
  }
    return 0;
}


/* condition L1
 *   iconst_0
 *   goto L2
 * L1:
 *   iconst_1
 * L2:
 *   dup
 * ifne L3
 *   pop
 * ------>
 * condition L1
 *   iconst_0
 *   goto L3
 * L1:
 *
 * This is equivalent since in the original code, the result of the first condition
 * dictates the result of "ifne L3" so we don't need to do all of this extra pushing and popping
 *
 * note that relative to simplify_label_chain_dup_pop1, we must do the opposite "condition"
 * in order for the new code to be sound
 */

int simplify_label_chain_dup_pop2(CODE **c)
{ int l1,l2,l3,l4,l5;
  int v1,v2;

  if (
    uses_label (*c, &l1)            && uniquelabel(l1) &&
    is_ldc_int (nextby(*c, 1), &v1) && v1 == 0 &&
    is_goto    (nextby(*c, 2), &l2) && uniquelabel(l2) &&
    is_label   (nextby(*c, 3), &l3) && l3 == l1 &&
    is_ldc_int (nextby(*c, 4), &v2) && v2 == 1 &&
    is_label   (nextby(*c, 5), &l4) && l4 == l2 &&
    is_dup     (nextby(*c, 6))      &&
    is_ifne    (nextby(*c, 7), &l5) &&
    is_pop     (nextby(*c, 8))
    ) {
        switch ((*c)->kind){
            case ifeqCK:
                return replace(c, 9,
                        makeCODEifne(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case ifneCK:
                return replace(c, 9,
                        makeCODEifeq(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case ifnullCK:
                return replace(c, 9,
                        makeCODEifnonnull(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case ifnonnullCK:
                return replace(c, 9,
                        makeCODEifnull(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpeqCK:
                return replace(c, 9,
                        makeCODEif_icmpne(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpneCK:
                return replace(c, 9,
                        makeCODEif_icmpeq(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpgtCK:
                return replace(c, 9,
                        makeCODEif_icmple(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpltCK:
                return replace(c, 9,
                        makeCODEif_icmpge(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpleCK:
                return replace(c, 9,
                        makeCODEif_icmpgt(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_icmpgeCK:
                return replace(c, 9,
                        makeCODEif_icmplt(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_acmpeqCK:
                return replace(c, 9,
                        makeCODEif_acmpne(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            case if_acmpneCK:
                return replace(c, 9,
                        makeCODEif_acmpeq(l1,
                        makeCODEldc_int(1,
                        makeCODEgoto(l5,
                        makeCODElabel(l1, NULL)
                    ))));
            default:
                return 0;
        }
  }
    return 0;
}


/* astore x
 * aload x
 * not aload x repeated
 * return
 * ------>
 * deleted
 * deleted
 * not aload x repeated
 * return
 */

int remove_unused_local_a(CODE **c)
{ int v,v2;
    /* here we store value on top of stack, then load again
     * which is not useful unless we use the stored value before overwriting it
     */
    if (is_astore(*c,&v) && is_aload(next(*c),&v2) && v != 0 && v == v2){
        int can_remove = 1;
        int local;
        int n = 2;
        CODE *line = next(next(*c));
        while (! (is_ireturn(line) || is_areturn(line) || is_empty(line)) ){
            if ((is_iload(line, &local) && local == v) || (is_aload(line, &local) && local == v)){
                /* then the stored local: at pos: v needs be stored since we load it later in the code. */
                can_remove = 0;
                break;
            }
            /* then we have overwritten the local at pos: v without using it */
            if ((is_istore(line, &local) && local == v) || (is_astore(line, &local) && local == v)){
                break;
            }
            line = next(line);
        }
        if (can_remove == 1){
            return replace(c,2, NULL);
        }
    }
    return 0;
}


int simplify_null_field(CODE **c)
{   int v;
    char *f;
    if (
    is_aconst_null ( *c) &&
    is_dup         ( nextby(*c, 1))   &&
    is_aload       ( nextby(*c, 2),&v) &&
    is_swap        ( nextby(*c, 3)) &&
    is_putfield    ( nextby(*c, 4),&f) &&
    is_pop         ( nextby(*c, 5))
    ){
        return replace(c,6,makeCODEaload(v,
                            makeCODEaconst_null(
                              makeCODEputfield(f,NULL))));
    }
  return 0;
}

/* int simplify_field(CODE **c) */
/* {   int v; */
/*     char *f; */
/*     if ( */
/*     is_aconst_null ( *c) && */
/*     is_dup         ( next(*c))   && */
/*     is_aload       ( next(next(*c)),&v) && */
/*     is_swap        ( next(next(next(*c)))) && */
/*     is_putfield    ( next(next(next(next(*c)))),&f) && */
/*     is_pop         ( next(next(next(next(next(*c)))))) */
/*     ){ */
/*         return replace(c,6,makeCODEaload(v, */
/*                             makeCODEaconst_null( */
/*                               makeCODEputfield(f,NULL)))); */
/*     } */
/*   return 0; */
/* } */


/* iload x        iload x        iload x
 * ldc 0          ldc 1          ldc 2
 * imul           imul           imul
 * ------>        ------>        ------>
 * ldc 0          iload x        iload x
 *                               dup
 *                               iadd
 */

int simplify_multiplication_right(CODE **c)
{ int x,k;
  if (is_iload(*c,&x) && 
      is_ldc_int(next(*c),&k) && 
      is_imul(next(next(*c)))) {
     if (k==0) return replace(c,3,makeCODEldc_int(0,NULL));
     else if (k==1) return replace(c,3,makeCODEiload(x,NULL));
     else if (k==2) return replace(c,3,makeCODEiload(x,
                                       makeCODEdup(
                                       makeCODEiadd(NULL))));
     return 0;
  }
  return 0;
}

/* iload x
 * ldc 0
 * iadd
 * ------>
 * iload x
 * 
 * This is equivalent since 0 is the additive identity.
 * The stack height is the same since both the original and the replacement increase it by 1.
 */

int simplify_addition_right(CODE **c)
{ int x,k;
  if (is_iload(*c,&x) && 
      is_ldc_int(next(*c),&k) && 
      is_iadd(next(next(*c)))) {
     if (k==0) return replace(c,3,makeCODEiload(x,NULL));
     return 0;
  }
  return 0;
}

/* iload x
 * ldc 0
 * isub
 * ------>
 * iload x
 * 
 * This is equivalent since 0 is the additive identity and 0 is on the right hand side.
 * The stack height is the same since both the original and the replacement increase it by 1.
 */

int simplify_subtraction_right(CODE **c)
{ int x,k;
  if (is_iload(*c,&x) && 
      is_ldc_int(next(*c),&k) && 
      is_isub(next(next(*c)))) {
     if (k==0) return replace(c,3,makeCODEiload(x,NULL));
     return 0;
  }
  return 0;
}

/* iload x
 * ldc 1
 * idiv
 * ------>
 * iload x
 * 
 * This is equivalent since 1 is the multiplicative identity and 1 is on the right hand side.
 * The stack height is the same since both the original and the replacement increase it by 1.
 */

int simplify_division_right(CODE **c)
{ int x,k;
  if (is_iload(*c,&x) && 
      is_ldc_int(next(*c),&k) && 
      is_idiv(next(next(*c)))) {
     if (k==1) return replace(c,3,makeCODEiload(x,NULL));
     return 0;
  }
  return 0;
}

/* dup
 * astore x
 * pop
 * -------->
 * astore x
 */
int simplify_astore(CODE **c)
{ int x;
  if (is_dup(*c) &&
      is_astore(next(*c),&x) &&
      is_pop(next(next(*c)))) {
     return replace(c,3,makeCODEastore(x,NULL));
  }
  return 0;
}

/* dup
 * istore x
 * pop
 * -------->
 * istore x
 * 
 * This is equivalent since the original copies what is on top of the stack, stores one in x and then pops the other.
 * The new code simply stores the original value into x and removes the useless duplicating.
 * The stack height is not affected since both decrease it by the same ammount.
 */
int simplify_istore(CODE **c)
{ int x;
  if (is_dup(*c) &&
      is_istore(next(*c),&x) &&
      is_pop(next(next(*c)))) {
     return replace(c,3,makeCODEistore(x,NULL));
  }
  return 0;
}

/* iload x
 * ldc k   (0<=k<=127)
 * iadd
 * istore x
 * --------->
 * iinc x k
 */ 
int positive_increment(CODE **c)
{ int x,y,k;
  if (is_iload(*c,&x) &&
      is_ldc_int(next(*c),&k) &&
      is_iadd(next(next(*c))) &&
      is_istore(next(next(next(*c))),&y) &&
      x==y && 0<=k && k<=127) {
     return replace(c,4,makeCODEiinc(x,k,NULL));
  }
  return 0;
}

/* goto L1
 * ...
 * L1:
 * goto L2
 * ...
 * L2:
 * --------->
 * goto L2
 * ...
 * L1:    (reference count reduced by 1)
 * goto L2
 * ...
 * L2:    (reference count increased by 1)  
 */
int simplify_goto_goto(CODE **c)
{ int l1,l2;
  if (is_goto(*c,&l1) && is_goto(next(destination(l1)),&l2) && l1>l2) {
     droplabel(l1);
     copylabel(l2);
     return replace(c,1,makeCODEgoto(l2,NULL));
  }
  return 0;
}

/* if_icmplt L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_icmpge L3
 * ...
 * 
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_icmplt(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_icmplt(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_icmpge(l5,NULL));
  }
  return 0;
}

/* if_icmple L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_icmpgt L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_icmple(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_icmple(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_icmpgt(l5,NULL));
  }
  return 0;
}

/* if_icmpgt L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_icmple L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_icmpgt(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_icmpgt(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_icmple(l5,NULL));
  }
  return 0;
}

/* if_icmpge L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_icmplt L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_icmpge(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_icmpge(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_icmplt(l5,NULL));
  }
  return 0;
}

/* if_icmpeq L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_icmpne L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_icmpeq(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_icmpeq(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_icmpne(l5,NULL));
  }
  return 0;
}

/* if_icmpne L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_icmpeq L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_icmpne(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_icmpne(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_icmpeq(l5,NULL));
  }
  return 0;
}

/* if_acmpeq L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_acmpne L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_acmpeq(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_acmpeq(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_acmpne(l5,NULL));
  }
  return 0;
}

/* ifeq L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * ifne L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_ifeq(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_ifeq(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEifne(l5,NULL));
  }
  return 0;
}

/* ifne L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * ifeq L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_ifne(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_ifne(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEifeq(l5,NULL));
  }
  return 0;
}

/* if_acmpne L1
 * iconst_0
 * goto L2
 * L1:
 * iconst_1
 * L2:
 * ifeq L3
 * ...
 * --------->
 * if_acmpeq L3
 * ...
 *  
 * This is equivalent since the code branches to L3 if the condition is false and continues if the condition is true.
 * Since the condition is inverted in the new code, it branches to L3 if the original condition is false, as it should.
 * There is no risk of removing a used label since the pattern only applies to unique labels whose unique reference is within the pattern.
 * The stack height is not affected since the original pushes a constant and pops is at the ifeq while the new code pushes nothing.
 */
int simplify_if_acmpne(CODE **c)
{ int l1,l2,l3,l4,l5,x,y;
  if (is_if_acmpne(*c,&l1) &&
      is_ldc_int(next(*c),&x) &&
      is_goto(next(next(*c)),&l2) &&
      is_label(next(next(next(*c))),&l3) && uniquelabel(l3) &&
      is_ldc_int(next(next(next(next(*c)))),&y) &&
      is_label(next(next(next(next(next(*c))))),&l4) && uniquelabel(l4) &&
      is_ifeq(next(next(next(next(next(next(*c)))))),&l5) &&
      x==0 && y==1 && l1==l3 && l2==l4) {
     droplabel(l3);
     droplabel(l4);
     return replace(c,7,makeCODEif_acmpeq(l5,NULL));
  }
  return 0;
}


/* return         ireturn        areturn
 * nop            nop            nop
 * ------>        ------>        ------>
 * return         ireturn        areturn
 * 
 * This is equivalent since the nop command is not required as a part of a branch and thus serves no purpose.
 * The stack height is unchanged since the return command is preserved.
 */
int remove_useless_nop(CODE **c) {
    if (is_nop(next(*c))) {
        switch((*c)->kind){
            case returnCK: return replace(c,2,makeCODEreturn(NULL));
            case ireturnCK: return replace(c,2,makeCODEireturn(NULL));
            case areturnCK: return replace(c,2,makeCODEareturn(NULL));
            default: return 0;
        }
    }
    return 0;
}

/* iload x
 * iload x
 * --------->
 * iload x
 * dup
 * 
 * This pattern simply replaces an identical load command with a dup command.
 * The stack height is not affected since both versions push the same number of things onto the stack.
 */
int simplify_duplication_iload(CODE **c){
    int x,y;
    if (is_iload(*c,&x) &&
        is_iload(next(*c),&y) &&
        x==y){
        return replace(c,2,makeCODEiload(x,makeCODEdup(NULL)));
    }
    return 0;
}

/* aload x
 * aload x
 * --------->
 * aload x
 * dup
 * 
 * This pattern simply replaces an identical load command with a dup command.
 * The stack height is not affected since both versions push the same number of things onto the stack.
 */
int simplify_duplication_aload(CODE **c){
    int x,y;
    if (is_aload(*c,&x) &&
        is_aload(next(*c),&y) &&
        x==y){
        return replace(c,2,makeCODEaload(x,makeCODEdup(NULL)));
    }
    return 0;
}

/* ldc x
 * ldc x
 * --------->
 * ldc x
 * dup
 * 
 * This pattern simply replaces an identical load command with a dup command.
 * The stack height is not affected since both versions push the same number of things onto the stack.
 */
int simplify_duplication_ldc(CODE **c){
    int x,y;
    if (is_ldc_int(*c,&x) &&
        is_ldc_int(next(*c),&y) &&
        x==y){
        return replace(c,2,makeCODEldc_int(x,makeCODEdup(NULL)));
    }
    return 0;
}

void init_patterns(void) {
	ADD_PATTERN(simplify_multiplication_right);
    ADD_PATTERN(simplify_addition_right);
    ADD_PATTERN(simplify_subtraction_right);
    ADD_PATTERN(simplify_division_right);
	ADD_PATTERN(simplify_astore);
    ADD_PATTERN(simplify_istore);
	ADD_PATTERN(positive_increment);
	ADD_PATTERN(simplify_goto_goto);
    ADD_PATTERN(simplify_if_icmplt);
    ADD_PATTERN(simplify_if_icmple);
    ADD_PATTERN(simplify_if_icmpgt);
    ADD_PATTERN(simplify_if_icmpge);
    ADD_PATTERN(simplify_if_icmpeq);
    ADD_PATTERN(simplify_if_icmpne);
    ADD_PATTERN(simplify_if_acmpeq);
    ADD_PATTERN(simplify_if_acmpne);
    ADD_PATTERN(simplify_ifeq);
    ADD_PATTERN(simplify_ifne);
    ADD_PATTERN(remove_unused_local_a);
    ADD_PATTERN(simplify_null_field);
    ADD_PATTERN(simplify_label_chain_dup_pop1);
    ADD_PATTERN(simplify_label_chain_dup_pop2);
    ADD_PATTERN(remove_useless_nop);
    ADD_PATTERN(simplify_duplication_iload);
    ADD_PATTERN(simplify_duplication_aload);
    ADD_PATTERN(simplify_duplication_ldc);
}
