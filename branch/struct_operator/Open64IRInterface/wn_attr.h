// -*-Mode: C++;-*-
// $Header: /home/derivs2/mstrout/CVSRepository/UseNewOA-Open64/Open64IRInterface/wn_attr.h,v 1.2 2006/03/16 03:48:55 mstrout Exp $

// * BeginCopyright *********************************************************
/*
  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan
*/
// *********************************************************** EndCopyright *

//***************************************************************************
//
// File:
//   $Source: /home/derivs2/mstrout/CVSRepository/UseNewOA-Open64/Open64IRInterface/wn_attr.h,v $
//
// Purpose:
//   [The purpose of this file]
//
// Description:
//   [The set of functions, macros, etc. defined in the file]
//
// Based on Open64 be/whirl2c/wn_attr.h
//
//***************************************************************************

#ifndef wn_attr_INCLUDED
#define wn_attr_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Description:
 *
 * Access-macros and access-functions for obtaining attributes of 
 * WN trees for general use in translating WN to another language
 * form (e.g. to C or Fortran).  For the most part, this module
 * supplements common/com/wn_core.h.
 *
 * Macros yielding WN attributes:
 * ------------------------------
 *
 *   WN_agoto_addr:
 *      TODO: is this an address or an index into a table of 
 *      addresses?
 *
 *   WN_condbr_cond:
 *      The conditional expression controlling the branch.
 *
 *   WN_compgoto_num_cases:
 *   WN_switch_num_cases:
 *      The number of cases in a computed goto statement (C switch
 *      statement).
 *
 *   WN_compgoto_idx:
 *   WN_switch_idx:
 *      The expression, which value is the index into the goto table.
 *
 *   WN_compgoto_table:
 *   WN_switch_table:
 *      The block of gotos representing the goto table.
 *
 *   WN_compgoto_has_default_case:
 *   WN_switch_has_default_case:
 *      TRUE if the computed goto is blessed with a default case.
 *
 *   WN_is_constant_expr:
 *      Return TRUE if we have an OPR_INTCONST or an OPR_CONST; otherwise
 *      return FALSE.
 *
 * Macros yielding OPCODE information:
 * -----------------------------------
 *
 *   INTR_is_adrtmp:
 *      An ADRTMP intrinsic opcode.
 *
 *   INTR_is_valtmp:
 *      An VALTMP intrinsic opcode.
 *
 * Functions yielding WN subtree information
 * ----------------------------------------
 *
 * WN_intrinsic_name:
 *      The name of the function or macro representing the intrinsic.
 *      Special handling is needed when the name is NULL.
 *
 * WN_intrinsic_return_ty:
 *      Map the intrinsic code to a return type.  If no return type
 *      is known, then use the result type of the OPCODE as the
 *      return type.
 *
 * WN_intrinsic_return_to_param:
 *      TRUE when the given return type is returned through the first
 *      parameter; FALSE when returning through pregs.
 *
 * Functions yielding WN subtree information
 * ----------------------------------------
 *
 *   WN_Get_PtrAdd_Intconst:
 *      Given the two operands of an ADD expression, see if this
 *      can be considered a pointer addition, and if it is return
 *      a pointer to an INTCONST that can be normalized w.r.t. the
 *      size of the pointed_ty; if the pointed_ty has size 1, then
 *      just return the integral expression operand when this should
 *      be considered a pointer addition; otherwise return NULL when
 *      this should not be considered a pointer addition.
 *
 *   WN_Tree_Type:
 *      The TY denoting the type of a WN expression.  For typeless
 *      WNs (e.g. statements), the TY will be "void" in C terminology.
 *      Similarly, there may be expression we cannot sensibly type,
 *      e.g. for MLOADs, in which case we return an inaccurate type.
 *      For MLOADs we return the type of a field at the given offset
 *      and of the given size, provided the size is known statically;
 *      otherwise we return the base-type.
 *
 * ====================================================================
 * ====================================================================
 */

//************************** System Include Files ***************************

//************************** Open64 Include Files ***************************

#include "Open64BasicTypes.h"

//*************************** User Include Files ****************************

//************************** Forward Declarations ***************************

//***************************************************************************

/* several craylib/dope items represent a no-op by a zero inconst...*/
#define IS_IO_NULL_OPR(wn) ((WN_operator(wn) == OPR_INTCONST) && (WN_const_val(wn) == 0))

#define WN_IOITEM(x) ((IOITEM) WN_io_item(x))
#define WN_IOSTMT(x) ((IOSTATEMENT) WN_io_statement(x))

#define IS_IO_ITEM_IOU(item) \
   (WN_IOITEM(item) >= IOU_NONE && WN_IOITEM(item) <= IOU_INTERNAL)
#define IS_IO_ITEM_IOF(item) \
   ((WN_IOITEM(item) >= IOF_NONE && WN_IOITEM(item) <= IOF_CR_FMTSRC_DOPE))
#define IS_IO_ITEM_IOC(item) \
   (WN_IOITEM(item) >= IOC_ACCESS && WN_IOITEM(item) <= IOC_ERRFLAG)
#define IS_IO_ITEM_IOL(item) \
   ((WN_IOITEM(item) >= IOL_ARRAY && WN_IOITEM(item) <= IOL_VAR) || \
    (WN_IOITEM(item) == IOL_DOPE))

//***************************************************************************

// FIXME: WE DON'T WANT THIS JUNK
#define WN_agoto_addr(wn) WN_kid0(wn)

#define WN_condbr_cond(wn) WN_kid0(wn)

#define WN_compgoto_num_cases(wn) WN_num_entries(wn)
#define WN_compgoto_idx(wn) WN_kid0(wn)
#define WN_compgoto_table(wn) WN_kid1(wn)
#define WN_compgoto_has_default_case(wn) (WN_kid_count(wn) == 3)

#define WN_switch_num_cases(wn) WN_num_entries(wn)
#define WN_switch_has_default_case(wn) (WN_kid_count(wn) == 3)

#define WN_is_constant_expr(wn) \
   (WN_operator(wn) == OPR_INTCONST || WN_operator(wn) == OPR_CONST)

#define INTR_is_adrtmp(intrn) \
   ((intrn) == INTRN_U4I1ADRTMP || \
    (intrn) == INTRN_U4I2ADRTMP || \
    (intrn) == INTRN_U4I4ADRTMP || \
    (intrn) == INTRN_U4I8ADRTMP || \
    (intrn) == INTRN_U4F4ADRTMP || \
    (intrn) == INTRN_U4F8ADRTMP || \
    (intrn) == INTRN_U4FQADRTMP || \
    (intrn) == INTRN_U4C4ADRTMP || \
    (intrn) == INTRN_U4C8ADRTMP || \
    (intrn) == INTRN_U4CQADRTMP || \
    (intrn) == INTRN_U4VADRTMP  || \
    (intrn) == INTRN_U8I1ADRTMP || \
    (intrn) == INTRN_U8I2ADRTMP || \
    (intrn) == INTRN_U8I4ADRTMP || \
    (intrn) == INTRN_U8I8ADRTMP || \
    (intrn) == INTRN_U8F4ADRTMP || \
    (intrn) == INTRN_U8F8ADRTMP || \
    (intrn) == INTRN_U8FQADRTMP || \
    (intrn) == INTRN_U8C4ADRTMP || \
    (intrn) == INTRN_U8C8ADRTMP || \
    (intrn) == INTRN_U8CQADRTMP || \
    (intrn) == INTRN_U8VADRTMP)

#define INTR_is_valtmp(intrn) \
   ((intrn) == INTRN_I4VALTMP || \
    (intrn) == INTRN_I8VALTMP || \
    (intrn) == INTRN_F4VALTMP || \
    (intrn) == INTRN_F8VALTMP || \
    (intrn) == INTRN_FQVALTMP || \
    (intrn) == INTRN_C4VALTMP || \
    (intrn) == INTRN_C8VALTMP || \
    (intrn) == INTRN_CQVALTMP)

#define WN_Skip_Parm(arg) \
   ((arg)!=NULL && WN_operator(arg) == OPR_PARM? WN_kid0(arg) : arg)


//***************************************************************************

extern TY_IDX 
WN_Tree_Type(const WN *wn);

extern TY_IDX 
Get_Field_Type(TY_IDX base, int field_id);

//***************************************************************************

// WN_GetRefObjType, WN_GetBaseObjType: For loads/stores, these are
// types for 1) the base object and 2) the referenced object.  The
// referenced object's address is at some offset (possibly 0) from the
// base object.  Loads refer the rvalue object; stores refer to the
// lvalue object.
extern TY_IDX 
WN_GetRefObjType(const WN* wn);

extern TY_IDX 
WN_GetBaseObjType(const WN* wn);


// Calls and intrinsic calls

extern TY_IDX 
WN_Call_Type(const WN* wn);

extern TY_IDX 
WN_Call_Return_Type(const WN* wn);

extern INT
WN_Call_First_Arg_Idx(const WN* wn);

extern INT
WN_Call_Last_Arg_Idx(const WN* wn);


extern const char * 
WN_intrinsic_name(INTRINSIC intr_opc);

extern TY_IDX 
WN_intrinsic_return_ty(const WN *call);

extern BOOL 
WN_intrinsic_return_to_param(TY_IDX return_ty);

//***************************************************************************

// Given a WN, return true if this is an ARRAY reference of some type.
// In other words, we look for an idiom of the forms:
//   ISTORE      or    ILOAD
//     ARRAY             ARRAY
// An ISTORE is interpreted as a handle for an lvalue; an ILOAD as a
// handle for an rvalue.
extern bool 
WN_isArrayRef(WN* wn);


extern WN *
WN_Get_PtrAdd_Intconst(WN* wn0, WN* wn1, TY_IDX pointed_ty);

//***************************************************************************

#endif /* wn_attr_INCLUDED */
