
// $Header$

// * BeginCopyright *********************************************************
// *********************************************************** EndCopyright *

//***************************************************************************
//
// File:
//   $Source$
//
// Purpose:
//   [The purpose of this file]
//
// Description:
//   [The set of functions, macros, etc. defined in the file]
//
//***************************************************************************

//************************** System Include Files ***************************

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>

//************************** Open64 Include Files ***************************

#include <Open64BasicTypes.h>

#include "cmplrs/rcodes.h"  // return codes
#include "tracing.h"        // trace routines
#include "ir_reader.h"      // fdump_tree

// finding out about common blocks
#include "stab_attr.h"

//*************************** User Include Files ****************************

#include "Args.hpp"
//#include "StrToHandle.hpp"

#include <Open64IRInterface.hpp>
#include <WhirlIO.h>
#include <diagnostics.h>
#include <OpenAnalysis/Utils/OutputBuilderDOT.hpp>
#include <OpenAnalysis/Alias/ManagerFIAliasAliasTag.hpp>
#include <OpenAnalysis/Alias/Interface.hpp>
#include <OpenAnalysis/CFG/ManagerCFG.hpp>
#include <OpenAnalysis/CallGraph/ManagerCallGraph.hpp>
#include <OpenAnalysis/ICFG/ManagerICFG.hpp>
#include <OpenAnalysis/CFG/EachCFGStandard.hpp>
#include <OpenAnalysis/DataFlow/ManagerParamBindings.hpp>
#include <OpenAnalysis/SideEffect/ManagerSideEffectStandard.hpp>
#include <OpenAnalysis/SideEffect/InterSideEffectStandard.hpp>
#include <OpenAnalysis/SideEffect/ManagerInterSideEffectStandard.hpp>
#include <OpenAnalysis/Activity/ManagerICFGDep.hpp>
#include <OpenAnalysis/Activity/ManagerICFGActive.hpp>
#include <OpenAnalysis/Activity/ManagerICFGUseful.hpp>
#include <OpenAnalysis/Activity/ManagerICFGVaryActive.hpp>
#include <OpenAnalysis/ReachConsts/ManagerICFGReachConsts.hpp>
#include <OpenAnalysis/ReachDefs/ManagerReachDefsStandard.hpp>
#include <OpenAnalysis/ReachDefsOverwrite/ManagerReachDefsOverwriteStandard.hpp>
#include <OpenAnalysis/UDDUChains/ManagerUDDUChainsStandard.hpp>
#include <OpenAnalysis/XAIF/ManagerAliasMapXAIF.hpp>
#include <OpenAnalysis/XAIF/ManagerUDDUChainsXAIF.hpp>

#include <sys/time.h>

//************************** Forward Declarations ***************************

static int
TestIR_OACFG_ForEachWNPU(std::ostream& os, PU_Info* pu,
                         OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAExprTree(std::ostream& os, PU_Info* pu,
                  OA::OA_ptr<Open64IRInterface> irInterface);



static int
TestIR_OAMemRefExpr_ForEachWNPU(std::ostream& os, PU_Info* pu,
                                OA::OA_ptr<Open64IRInterface> irInterface);



static int
TestIR_OACallGraph(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);



static int
TestIR_OAICFG(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface);



static int
TestIR_OAParamBindings(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OASideEffect(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);



static int
TestIR_OAICFGDep(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAICFGActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);



static int
TestIR_OAICFGReachConsts(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface);



static int
TestIR_OAICFGReachDefs(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAUDDUChains(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAAliasMapXAIF(std::ostream& os, PU_Info* pu_forest,
                      OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAICFGReachDefsOverwrite(std::ostream& os, PU_Info* pu_forest,
                                OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAUDDUChainsXAIF(std::ostream& os, PU_Info* pu_forest,
                        OA::OA_ptr<Open64IRInterface> irInterface);


//***************************************************************************

int
main(int argc, char* argv[])
{
  // -------------------------------------------------------
  // 1. Open64 Initialization
  // -------------------------------------------------------
  Handle_Signals();
  MEM_Initialize();
  Init_Error_Handler( 100 );
  Set_Error_Line( ERROR_LINE_UNKNOWN );
  Set_Error_File( NULL );
  Set_Error_Phase("whirltester");
  IR_set_dump_order(TRUE /*pre*/); // pre-order trees when debugging, please!
  
  Preconfigure();         // from config.cxx...
  Configure();            // needed for WN_lower!
  Configure_Source(NULL); // Most config variables set here

  Init_Operator_To_Opcode_Table(); // FIXME
    
  // -------------------------------------------------------
  // 2. Local initialization (options, etc.)
  // -------------------------------------------------------
  Diag_Init();
  Diag_Set_Max_Diags(100); // Maximum 100 warnings by default

  
  Args args(argc, argv);

  // -------------------------------------------------------
  // 3. Read WHIRL IR
  // -------------------------------------------------------
  //PU_Info* pu_forest = ReadIR(whirlFileNm);
  PU_Info* pu_forest = ReadIR(args.whirlFileNm.c_str());
  PrepareIR(pu_forest); // used in whirl2xaif, xaif2whirl
  // -------------------------------------------------------
  // 4. Do something
  // -------------------------------------------------------  
  
  if (args.dumpIR) { 
    DumpIR(pu_forest); 
  }
  OA::OA_ptr<Open64IRInterface> irInterface; 
  irInterface = new Open64IRInterface();
  Open64IRInterface::initContextState(pu_forest);



  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //! Per procedure analysis
  procIter->reset();
  for ( ; procIter->isValid(); ++(*procIter)) {

    // The PU_Info* for this PU
    PU_Info* pu = (PU_Info*)procIter->current().hval();

    switch (args.runMode)
    {
      case 1:
      {
        //! Control FLow Graph
        TestIR_OACFG_ForEachWNPU(std::cout, pu, irInterface);
        break;
      }

      case 2:
      {
        //! Memory Reference Expressions
        TestIR_OAMemRefExpr_ForEachWNPU(std::cout, pu, irInterface);
        break;
      }

      case 4:
      {
        //! Expression Tree
        TestIR_OAExprTree(std::cout, pu, irInterface);
        break;
      }
    }
  }

  //! Interprocedural Analysis
  procIter->reset();
  switch (args.runMode)
  {
     case 3:
     {
        //! FIAlias
        OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
        fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
        OA::OA_ptr<OA::Alias::Interface> alias;
        alias = fialiasman->performAnalysis(procIter);
        alias->output(*irInterface);
        break;
     }

     case 5:
     {
        //! CallGraph
        TestIR_OACallGraph(std::cout, pu_forest, irInterface);
        break; 
     }

     case 6:
     {
        //! ICFG
        TestIR_OAICFG(std::cout, pu_forest, irInterface);
        break;
     }

     case 7:
     {
        //! ParamBindings
        TestIR_OAParamBindings(std::cout, pu_forest, irInterface);
        break;
     }

     case 8:
     {
        TestIR_OASideEffect(std::cout, pu_forest, irInterface);
        break;
     }

     case 9:
     {
        TestIR_OAICFGDep(std::cout, pu_forest, irInterface);
        break;
     }

     case 10:
     {
        TestIR_OAICFGActivity(std::cout, pu_forest, irInterface);
        break;
     }

     case 11:
     {
        TestIR_OAICFGReachConsts(std::cout, pu_forest, irInterface);
        break;
     }

     case 12:
     {
        TestIR_OAICFGReachDefs(std::cout, pu_forest, irInterface);
        break;
     }
    
     case 13:
     {
        TestIR_OAUDDUChains(std::cout, pu_forest, irInterface);
        break;
     }


     case 14:
     {
        TestIR_OAAliasMapXAIF(std::cout, pu_forest, irInterface);
        break;
     }


     case 15:
     {
        TestIR_OAUDDUChainsXAIF(std::cout, pu_forest, irInterface);
        break;
     }

     case 16:
     {
        TestIR_OAICFGReachDefsOverwrite(std::cout, pu_forest, irInterface);
        break;
     }

 
  }

  FreeIR(pu_forest); // N.B. cannot use with WriteIR

  // -------------------------------------------------------
  // 5. Finalization
  // -------------------------------------------------------

  // If we've seen errors, note them and terminate
  INT local_ecount, local_wcount;
  if ( Get_Error_Count ( &local_ecount, &local_wcount ) ) {
    Terminate(Had_Internal_Error() ? RC_INTERNAL_ERROR : 
              RC_NORECOVER_USER_ERROR);
  }

  Diag_Exit();
  Cleanup_Files(TRUE, FALSE); // Open64

  return RC_OKAY;
}


//! =================================================================
//! Control Flow Graph
//! =================================================================

static int
TestIR_OACFG_ForEachWNPU(std::ostream& os, PU_Info* pu,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OACFG");

  OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgmanstd;
  cfgmanstd = new OA::CFG::ManagerCFGStandard(irInterface);
  OA::OA_ptr<OA::CFG::CFG> cfg
      = cfgmanstd->performAnalysis((OA::irhandle_t)pu);

  // text output
  OA::OA_ptr<OA::OutputBuilder> outBuildText;
  outBuildText = new OA::OutputBuilderText;
  cfg->configOutput(outBuildText);
  cfg->output(*irInterface);
  // dot output
  std::cout << "--------Dot Graph Output--------" << std::endl;
  OA::OA_ptr<OA::OutputBuilder> outBuildDot;
  outBuildDot = new OA::OutputBuilderDOT;
  cfg->configOutput(outBuildDot);
  cfg->output(*irInterface);

  return 0;
}


//! =============================================================
//! Expression Tree
//! =============================================================

static int
TestIR_OAExprTree(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> ir)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAExprTree");

  // iterate over all statements
  //   iterate over ExprHandles for the statement
  //     create ExprTree(expr)


  OA::OA_ptr<OA::IRStmtIterator> sIt = ir->getStmtIterator((OA::irhandle_t)pu);
  for ( ; sIt->isValid(); (*sIt)++) {
    OA::StmtHandle stmt = sIt->current();

    std::cout << "============================================" << std::endl;
    std::cout << "StmtHandle: ";
    // do not combine the line before and after this comment, with
    // some g++ compilers that can result in certain statements being
    // printed out before the OA::StmtHandle label, who knows MMS
    std::cout << ir->toString(stmt) << std::endl << std::endl;

    OA::OA_ptr<OA::ExprHandleIterator> exprIter;
    exprIter = ir->getExprHandleIterator(stmt);
    for ( ; exprIter->isValid(); (*exprIter)++) {
         OA::ExprHandle expr = exprIter->current();
         std::cout << "==> ExprHandle: ";
         std::cout << ir->toString(expr) << std::endl;
         OA::OA_ptr<OA::ExprTree> eTreePtr = ir->getExprTree(expr);
         eTreePtr->output(*ir);
         std::cout << std::endl << std::endl;
    }
  }
  return 0;

}



//! ===================================================================
//! Memory Reference Expressions
//! StrictlyLocal Field of NamedRef.
//! ===================================================================

static int
TestIR_OAMemRefExpr_ForEachWNPU(std::ostream& os, PU_Info* pu,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  OA::OA_ptr<Open64IRInterface> ir; ir = new Open64IRInterface();
  OA::ProcHandle proc((OA::irhandle_t)pu);

  std::cout << "======================================" << std::endl;
  
  std::cout << "ProcHandle : " << irInterface->toString(proc) << std::endl;
  
  // for each statement stmt
  //   for each memory reference in stmt
  //     for each mem-ref-expr for the memory reference
  //        output information about the memory reference expressions
  OA::OA_ptr<OA::IRStmtIterator> sIt = ir->getStmtIterator(proc);
  for ( ; sIt->isValid(); (*sIt)++) {
    OA::StmtHandle stmt = sIt->current();

    std::cout << std::endl << std::endl << std::endl;
    std::cout << "OA::StmtHandle: ";
    // do not combine the line before and after this comment, with
    // some g++ compilers that can result in certain statements being
    // printed out before the OA::StmtHandle label, who knows MMS
    std::cout << ir->toString(stmt) << std::endl;
    //ir->dump(stmt, std::cout);
    //std::cout << "AliasStmtType: "
    //      << ir->toString(ir->getAliasStmtType(stmt)) << std::endl;
    //std::cout << std::endl;

    OA::OA_ptr<OA::MemRefHandleIterator> mrIt = ir->getAllMemRefs(stmt);
    for ( ; mrIt->isValid(); (*mrIt)++) {
      OA::MemRefHandle memref = mrIt->current();

      std::cout << "MemRefHandle" << ir->toString(memref) << std::endl;

      // print out the debugging information to give some context
      //std::cout << "  ***** OA::MemRefHandle: ";
      //ir->dump(memref, std::cout);

      // get and print the memory reference expressions for this handle
      OA::OA_ptr<OA::MemRefExprIterator> mreIter
          = ir->getMemRefExprIterator(memref);
      for ( ; mreIter->isValid(); (*mreIter)++ ) {
        OA::OA_ptr<OA::MemRefExpr::MemRefExpr> mre = mreIter->current();
        std::cout << "\tmre = ";
        mre->output(*ir);
        std::cout << std::endl;
      }
    }
  }

  std::cout << "======================================" << std::endl;
 
  return 0;
}



//! =================================================================
//! CallGraph
//! ================================================================

static int
TestIR_OACallGraph(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OACallGraph");

  //! Get the ProcIter
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //! FIAliasAliasMap
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
  OA::OA_ptr<OA::Alias::Interface> alias;
  alias = fialiasman->performAnalysis(procIter);
  alias->output(*irInterface);

  //! CallGraph
  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph;
  cgraph =  cgraphman->performAnalysis(procIter,alias);

  //! CallGraph text output
  cgraph->output(*irInterface);
  //! CallGraph dot output
  OA::OA_ptr<OA::OutputBuilder> outBuild;
  outBuild = new OA::OutputBuilderDOT;
  cgraph->configOutput(outBuild);
  cgraph->output(*irInterface);

  return 0;
}



//! ====================================================================
//! Interprocedural Control Flow Graph
//! =====================================================================

static int
TestIR_OAICFG(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{

    Diag_Set_Phase("WHIRL tester: TestIR_OAICFG");

    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = new OA::ICFG::ICFG();

    //===========================================================
    // Have the manager create one from the CFGs

    // CFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    alias->output(*irInterface);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    // ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);

    // text output
    icfg->output(*irInterface);
    // dot output
    OA::OA_ptr<OA::OutputBuilder> outBuild;
    outBuild = new OA::OutputBuilderDOT;
    icfg->configOutput(outBuild);
    icfg->output(*irInterface);

}



//! ===============================================================
//! ParamBindings
//! ===============================================================


static int
TestIR_OAParamBindings(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAParamBindings");

  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //! FIAliasAliasMap
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
  OA::OA_ptr<OA::Alias::Interface> alias;
  alias = fialiasman->performAnalysis(procIter);

  //! CallGraph
  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

  //! AliasMap
  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
  pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
  parambind = pbman->performAnalysis(cgraph);
  parambind->output(*irInterface);

  return 0;
}



//! ================================================================
//! SideEffect
//! ==================================================================

static int
TestIR_OASideEffect(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAInterSideEffect");

  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //! FIAliasAliasMap
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
  OA::OA_ptr<OA::Alias::Interface> alias;
  alias = fialiasman->performAnalysis(procIter);
  OA::OA_ptr<OA::Alias::InterAliasResults> interAlias;
  interAlias = new OA::Alias::InterAliasResults(alias);


  //! CallGraph
  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
  pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
  parambind = pbman->performAnalysis(cgraph);


/*
  //! SideEffect
  // Create empty interprocedural side-effect analysis results and
  // empty intraprocedural
  // results
  OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffectInter;
  interSideEffectInter = new OA::SideEffect::InterSideEffectStandard(interAlias);

  OA::OA_ptr<OA::SideEffect::SideEffectStandard> results;

  // Perform the analyis on each procedure.
  OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> sideeffectman;
  sideeffectman = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

  for(procIter->reset(); procIter->isValid(); ++(*procIter)) {
      OA::ProcHandle proc = procIter->current();
      results = sideeffectman->performAnalysis(proc, alias, 
                                               interSideEffectInter);
      results->output(*irInterface,*alias);
  }
*/



  // Intra Side-Effect
  OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> sideeffectman;
  sideeffectman = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

  // InterSideEffect
  OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
  interSEman = new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);

  OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
  interSE = interSEman->performAnalysis(cgraph, parambind,
                                        interAlias, sideeffectman,             
                                        OA::DataFlow::ITERATIVE);
//  interSE->output(*irInterface,*alias);
 
  return 0;
}



//! ==============================================================
//! ICFGDep
//! ==============================================================

static int
TestIR_OAICFGDep(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

    std::cout << " Dep Analysis " << std::endl;

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    // eachCFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);


    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    //alias->output(*irInterface);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);

    // ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);


    //ICFGDep
    OA::OA_ptr<OA::Activity::ManagerICFGDep> icfgdepman;
    icfgdepman = new OA::Activity::ManagerICFGDep(irInterface);
    OA::OA_ptr<OA::Activity::ICFGDep> icfgDep;
    icfgDep = icfgdepman->performAnalysis(icfg, parambind, alias,
                                        OA::DataFlow::ITERATIVE);
    icfgDep->output(*irInterface, *alias);

    return 0;
}



//! ==============================================================
//! ICFGActivity
//! ==============================================================


static int
TestIR_OAICFGActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{
    timeval tim;
    double t1,t2;

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //! =================================
    //! Control Flow Graph Timings
    //! ================================

    // eachCFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    //! ================================
    //! AliasMapFIAlias Timings
    //! ================================

    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    //alias->output(*irInterface);
    
    std::cout << "Done Alias" << std::endl;

    //! =================================
    //! CallGraph Timings
    //! =================================

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    std::cout << "Done CallGraph" << std::endl;

    //! =====================================
    //! ParamBindings
    //! =====================================

    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);

    std::cout << "Done ParamBindings" << std::endl;

    //! =====================================

    // ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);

    std::cout << "Done ICFG" << std::endl;


    //ICFGDep
    OA::OA_ptr<OA::Activity::ManagerICFGDep> icfgdepman;
    icfgdepman = new OA::Activity::ManagerICFGDep(irInterface);
    OA::OA_ptr<OA::Activity::ICFGDep> icfgDep;
    icfgDep = icfgdepman->performAnalysis(icfg, parambind, alias,
                                        OA::DataFlow::ITERATIVE);

    //icfgDep->output(*irInterface,*alias); 
    
    std::cout << "Done ICFGDep" << std::endl;

    std::cout << "before Useful" << std::endl;

    // ICFGUseful   (for testing)
    OA::OA_ptr<OA::Activity::ManagerICFGUseful> usefulman;
    usefulman = new OA::Activity::ManagerICFGUseful(irInterface);
    OA::OA_ptr<OA::Activity::InterUseful> icfgUseful;
    icfgUseful = usefulman->performAnalysis(icfg, parambind, alias,
                                            icfgDep, OA::DataFlow::ITERATIVE);
//    icfgUseful->output(*irInterface);

/*
    std::cout << "after Useful" << std::endl;

    // ICFGVaryActive
    OA::OA_ptr<OA::Activity::ManagerICFGVaryActive> varyman;
    varyman = new OA::Activity::ManagerICFGVaryActive(irInterface);
    OA::OA_ptr<OA::Activity::ActivePerStmt> inActive;
    inActive = varyman->performAnalysis(icfg, parambind, alias,
                                icfgDep, icfgUseful, OA::DataFlow::ITERATIVE);
  
    inActive->output(*irInterface);


    std::cout << "after ICFGVaryActive" << std::endl;
*/

/*
    //! ====================================
    //! ICFGActivity
    //! ====================================

    OA::OA_ptr<OA::Activity::ManagerICFGActive> activeman;
    activeman = new OA::Activity::ManagerICFGActive(irInterface);
    OA::OA_ptr<OA::Activity::InterActiveFortran> active;
    active = activeman->performAnalysis(icfg, parambind,
                                        alias, OA::DataFlow::ITERATIVE);

    std::cout << "Output of Activity Analysis" << std::endl;
    active->output(*irInterface,*alias);
*/
}


//! ===============================================================
//! ICFGReachConst
//! ===============================================================


static int
TestIR_OAICFGReachConsts(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAICFGReachConsts");

    // eachCFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    OA::OA_ptr<OA::Alias::InterAliasResults> interAlias;
    interAlias = new OA::Alias::InterAliasResults(alias);

    //alias->output(*irInterface);

    //interAlias->output(*irInterface);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    //cgraph->dump(std::cout, irInterface);


    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);


    // ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);


    // intra side effects
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> intraSideEffectMgr;
    intraSideEffectMgr
        = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

    // inter side effects
    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSideEffects;
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSideEffectMgr;
    interSideEffectMgr =
        new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
    interSideEffects = interSideEffectMgr->performAnalysis(
        cgraph,
        parambind,
        interAlias,
        intraSideEffectMgr,
        OA::DataFlow::ITERATIVE);


/*
    // ICFGReachConsts
    OA::OA_ptr<OA::ReachConsts::ManagerICFGReachConsts> ircsman;
    ircsman = new OA::ReachConsts::ManagerICFGReachConsts(irInterface);
    OA::OA_ptr<OA::ReachConsts::InterReachConsts> ircs
        = ircsman->performAnalysis(icfg, alias, OA::DataFlow::ITERATIVE);
 
    ircs->output(*irInterface,*alias);
*/
    return 0;
}



//! ==================================================================
//! ICFGReachDefs
//! ==================================================================



static int
TestIR_OAICFGReachDefs(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
    std::cout << "Test reaching defs analysis\n";

    // CFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);


    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    OA::OA_ptr<OA::Alias::InterAliasResults> interAlias;
    interAlias = new OA::Alias::InterAliasResults(alias);


    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);


   //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);


    // intra side effects
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> intraSideEffectMgr;
    intraSideEffectMgr
        = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

    // inter side effects
    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSideEffects;
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSideEffectMgr;
    interSideEffectMgr =
        new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
    interSideEffects = interSideEffectMgr->performAnalysis(
        cgraph,
        parambind,
        interAlias,
        intraSideEffectMgr,
        OA::DataFlow::ITERATIVE);


    // Reaching Defs
    OA::OA_ptr<OA::ReachDefs::ReachDefsStandard> reachDefResults;
    OA::OA_ptr<OA::ReachDefs::ManagerReachDefsStandard> reachDefMgr;
    reachDefMgr = new OA::ReachDefs::ManagerReachDefsStandard(irInterface);

    for(procIter->reset(); procIter->isValid(); ++(*procIter))
    {
        OA::ProcHandle proc = procIter->current();

        reachDefResults = reachDefMgr->performAnalysis(
            proc,
            eachCFG->getCFGResults(proc),
            alias,
            interSideEffects,
            OA::DataFlow::ITERATIVE);

        reachDefResults->output(*irInterface);
        std::cout << std::endl;
    }

    return 0;
}




//! =========================================================================
//! ReachDefsOverwrite,  Needed by OpenAD 
//! =========================================================================

static int
TestIR_OAICFGReachDefsOverwrite(std::ostream& os, PU_Info* pu_forest,
                                OA::OA_ptr<Open64IRInterface> irInterface)
{
    std::cout << "Test reaching defs analysis\n";

    // CFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);


    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    OA::OA_ptr<OA::Alias::InterAliasResults> interAlias;
    interAlias = new OA::Alias::InterAliasResults(alias);


    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);


   //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);


   // intra side effects
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> intraSideEffectMgr;
    intraSideEffectMgr
        = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

    // inter side effects
    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSideEffects;
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSideEffectMgr;
    interSideEffectMgr =
        new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
    interSideEffects = interSideEffectMgr->performAnalysis(
        cgraph,
        parambind,
        interAlias,
        intraSideEffectMgr,
        OA::DataFlow::ITERATIVE);


    // Reaching Defs
    OA::OA_ptr<OA::ReachDefsOverwrite::ReachDefsOverwriteStandard> reachDefResults;

    OA::OA_ptr<OA::ReachDefsOverwrite::ManagerReachDefsOverwriteStandard> 
                                           reachDefsOverwriteMgr;

    reachDefsOverwriteMgr 
         = new OA::ReachDefsOverwrite::ManagerReachDefsOverwriteStandard(irInterface);

    for(procIter->reset(); procIter->isValid(); ++(*procIter))
    {
        OA::ProcHandle proc = procIter->current();

        reachDefResults = reachDefsOverwriteMgr->performAnalysis(
            proc,
            eachCFG->getCFGResults(proc),
            alias,
            interSideEffects,
            OA::DataFlow::ITERATIVE);

        reachDefResults->output(*irInterface);
        std::cout << std::endl;
    }

    return 0;
}



//! =================================================================
//! UDDUChains
//! =================================================================


static int
TestIR_OAUDDUChains(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
    std::cout << "Test UDDUChains analysis\n";

    // CFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);


    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    OA::OA_ptr<OA::Alias::InterAliasResults> interAlias;
    interAlias = new OA::Alias::InterAliasResults(alias);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);


    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);


    // intra side effects
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> intraSideEffectMgr;
    intraSideEffectMgr
        = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

    // inter side effects
    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSideEffects;
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSideEffectMgr;
    interSideEffectMgr =
        new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
    interSideEffects = interSideEffectMgr->performAnalysis(
        cgraph,
        parambind,
        interAlias,
        intraSideEffectMgr,
        OA::DataFlow::ITERATIVE);

    // Reaching Defs
    OA::OA_ptr<OA::ReachDefs::ReachDefsStandard> reachDefResults;
    OA::OA_ptr<OA::ReachDefs::ManagerReachDefsStandard> reachDefMgr;
    reachDefMgr = new OA::ReachDefs::ManagerReachDefsStandard(irInterface);

    OA::OA_ptr<OA::UDDUChains::UDDUChainsStandard> udduchains;
    OA::OA_ptr<OA::UDDUChains::ManagerUDDUChainsStandard> udman;
    udman = new OA::UDDUChains::ManagerUDDUChainsStandard(irInterface);

    for(procIter->reset(); procIter->isValid(); ++(*procIter))
    {
        OA::ProcHandle proc = procIter->current();

        reachDefResults = reachDefMgr->performAnalysis(
            proc,
            eachCFG->getCFGResults(proc),
            alias,
            interSideEffects,
            OA::DataFlow::ITERATIVE);


        udduchains = udman->performAnalysis(proc,
                                           alias,
                                           reachDefResults,
                                           interSideEffects);

        udduchains->output(*irInterface);
     }

    return 0;
}



//! ===============================================================
//! AliasMapXAIF, Needed by OpenAD
//! ===============================================================

static int
TestIR_OAAliasMapXAIF(std::ostream& os, PU_Info* pu_forest,
                        OA::OA_ptr<Open64IRInterface> irInterface)
{
    std::cout << "Test AliasMapXAIF analysis\n";

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);


    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);


    // XAIF AliasTag

    OA::OA_ptr<OA::XAIF::ManagerAliasMapXAIF> aliasmapxaifman;
    aliasmapxaifman = new OA::XAIF::ManagerAliasMapXAIF(irInterface);

    // Following for loop will generate aliasMapXAIF for each procedure.
    OA::OA_ptr<OA::XAIF::AliasMapXAIF> aliasMapXAIF;

    for(procIter->reset(); procIter->isValid(); ++(*procIter))
    {
        OA::ProcHandle proc = procIter->current();
        aliasMapXAIF = aliasmapxaifman->performAnalysis(proc,alias);
        aliasMapXAIF->output(*irInterface);
    }

    return 0;
}



//! ===================================================================
//! UDDUChainsXAIF
//! ===================================================================

static int
TestIR_OAUDDUChainsXAIF(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
    std::cout << "Test UDDUChains analysis\n";

    // CFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);


    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);
    OA::OA_ptr<OA::Alias::InterAliasResults> interAlias;
    interAlias = new OA::Alias::InterAliasResults(alias);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);


    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);

    // intra side effects
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> intraSideEffectMgr;
    intraSideEffectMgr
        = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

    // inter side effects
    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSideEffects;
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSideEffectMgr;
    interSideEffectMgr =
        new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
    interSideEffects = interSideEffectMgr->performAnalysis(
        cgraph,
        parambind,
        interAlias,
        intraSideEffectMgr,
        OA::DataFlow::ITERATIVE);

    // Reaching Defs
    OA::OA_ptr<OA::ReachDefs::ReachDefsStandard> reachDefResults;
    OA::OA_ptr<OA::ReachDefs::ManagerReachDefsStandard> reachDefMgr;
    reachDefMgr = new OA::ReachDefs::ManagerReachDefsStandard(irInterface);


    OA::OA_ptr<OA::UDDUChains::UDDUChainsStandard> udduchains;
    OA::OA_ptr<OA::UDDUChains::ManagerUDDUChainsStandard> udman;
    udman = new OA::UDDUChains::ManagerUDDUChainsStandard(irInterface);


    OA::OA_ptr<OA::XAIF::UDDUChainsXAIF> udduchainsXAIF;
    OA::OA_ptr<OA::XAIF::ManagerUDDUChainsXAIF> udmanXAIF;
    udmanXAIF = new OA::XAIF::ManagerUDDUChainsXAIF(irInterface);


    for(procIter->reset(); procIter->isValid(); ++(*procIter))
    {
        OA::ProcHandle proc = procIter->current();

        reachDefResults = reachDefMgr->performAnalysis(
            proc,
            eachCFG->getCFGResults(proc),
            alias,
            interSideEffects,
            OA::DataFlow::ITERATIVE);


        udduchains = udman->performAnalysis(proc,
                                           alias,
                                           reachDefResults,
                                           interSideEffects);


        udduchainsXAIF =
        udmanXAIF->performAnalysis(eachCFG->getCFGResults(proc), 
                                   udduchains, true);




        //udduchainsXAIF->dump(std::cout, irInterface);
        udduchainsXAIF->output(*irInterface);



     }


    return 0;
}


