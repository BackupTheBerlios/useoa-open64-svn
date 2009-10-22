
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
#include "timer.h"

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
#include <OpenAnalysis/Activity/ManagerICFGActive.hpp>
#include <OpenAnalysis/Activity/ManagerICFGUseful.hpp>
#include <OpenAnalysis/Activity/ManagerICFGVaryActive.hpp>
#include <OpenAnalysis/ReachConsts/ManagerICFGReachConsts.hpp>
#include <OpenAnalysis/ReachConsts/ManagerICFGCSReachConsts.hpp>
#include <OpenAnalysis/ReachDefs/ManagerReachDefsStandard.hpp>
#include <OpenAnalysis/ReachDefsOverwrite/ManagerReachDefsOverwriteStandard.hpp>
#include <OpenAnalysis/UDDUChains/ManagerUDDUChainsStandard.hpp>
/*
#include <OpenAnalysis/XAIF/ManagerAliasMapXAIF.hpp>
#include <OpenAnalysis/XAIF/ManagerUDDUChainsXAIF.hpp>
#include <OpenAnalysis/CSFIActivity/ManagerDUGStandard.hpp>
#include <OpenAnalysis/CSFIActivity/ManagerDUActive.hpp>
*/
#include <OpenAnalysis/Alias/ManagerCallContexts.hpp>
#include <OpenAnalysis/Alias/ManagerCSFIAliasAliasTag.hpp>
#include <OpenAnalysis/Activity/ManagerICFGCSUseful.hpp>
#include <OpenAnalysis/Activity/ManagerICFGCSVaryActive.hpp>
#include <OpenAnalysis/Activity/ManagerICFGCSActive.hpp>



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
TestIR_OAICFGActivity(std::ostream& os, PU_Info* pu_forest,
                      OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAICFGReachConsts(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAICFGCSReachConsts(std::ostream& os, PU_Info* pu_forest,
                           OA::OA_ptr<Open64IRInterface> irInterface,
                           int ccmax);


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



static int
TestIR_OACSFIActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OAsac07ICFGActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OACSFIAliasAliasTag(std::ostream& os, PU_Info* pu_forest,
                           OA::OA_ptr<Open64IRInterface> irInterface,
                           int ccmax);


static int
TestIR_OAICFGCSActivity(std::ostream& os, PU_Info* pu_forest,
                        OA::OA_ptr<Open64IRInterface> irInterface,
                        int ccmax);


static int
TestIR_OACallContexts(std::ostream& os, PU_Info* pu_forest,
                      OA::OA_ptr<Open64IRInterface> irInterface,
                      int ccmax);




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

     case 32:
     {
        TestIR_OACSFIActivity(std::cout, pu_forest, irInterface);
        break;
     }

     case 37:
     {
        TestIR_OAsac07ICFGActivity(std::cout, pu_forest, irInterface);
        break;
     }
 
     case 38:
     {
       TestIR_OACSFIAliasAliasTag(std::cout, pu_forest, irInterface,
                                  args.ccmax);
        break;
     }

     case 39:
     {
        TestIR_OAICFGCSActivity(std::cout, pu_forest, irInterface,
                                args.ccmax);
        break;
     }

     case 40:
     {
        TestIR_OAICFGCSReachConsts(std::cout, pu_forest, irInterface,
                                   args.ccmax);
        break;
     }
     case 41:
     {
        //! CallContexts
        TestIR_OACallContexts(std::cout, pu_forest, irInterface,
                              args.ccmax);
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



//! =================================================================
//! CallContexts
//! ================================================================

static int
TestIR_OACallContexts(std::ostream& os, PU_Info* pu_forest,
                      OA::OA_ptr<Open64IRInterface> irInterface,
                      int ccmax)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OACallContexts");

  //! Get the ProcIter
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //! FIAliasAliasMap
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
  OA::OA_ptr<OA::Alias::Interface> alias;
  alias = fialiasman->performAnalysis(procIter);
  //alias->output(*irInterface);

  //! CallGraph
  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph;
  cgraph =  cgraphman->performAnalysis(procIter,alias);
  cgraph->output(*irInterface);

  //! CallContexts

  // set CallContext k-level to ccmax
  OA::Alias::CallContext::setMaxDegree((unsigned int) ccmax);
  std::cout << "CallContext::setMaxDegree(" << ccmax << ")\n\n";

  OA::OA_ptr<OA::Alias::ManagerCallContexts> ccman;
  ccman = new OA::Alias::ManagerCallContexts(irInterface);
  OA::OA_ptr<OA::Alias::CCSetPerProc> ccResults;
  ccResults = ccman->performAnalysis(cgraph);

  //! CallContexts text output
  ccResults->output(*irInterface);

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

  //! ParamBindings
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


  //! Intra Side-Effect
  OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> sideeffectman;
  sideeffectman = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

  // InterSideEffect
  OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
  interSEman = new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);

  OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
  interSE = interSEman->performAnalysis(cgraph, parambind,
                                        interAlias, sideeffectman,             
                                        OA::DataFlow::ITERATIVE);
  interSE->output(*irInterface,*alias);
 
  return 0;
}



//! ==============================================================
//! ICFGActivity
//! ==============================================================


static int
TestIR_OAICFGActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{
    //timeval tim;
    //double t1,t2;

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //! =================================
    //! Control Flow Graph Timings
    //! ================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    //! eachCFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf CFG seconds elapsed\n", t2-t1);


    //! ================================
    //! AliasTagFIAlias Timings
    //! ================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf AliasTagFIAlias seconds elapsed\n", t2-t1);

    
    //! =================================
    //! CallGraph Timings
    //! =================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf CallGraph seconds elapsed\n", t2-t1);


    //! =====================================
    //! ParamBindings
    //! =====================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf ParamBindings seconds elapsed\n", t2-t1);

    //! =====================================
    //! ICFG
    //! =====================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf ICFG seconds elapsed\n", t2-t1);

    //! =====================================
    //! ICFGUseful   (for testing)
    //! =====================================

    // OA::OA_ptr<OA::Activity::ManagerICFGUseful> usefulman;
    // usefulman = new OA::Activity::ManagerICFGUseful(irInterface);
    // OA::OA_ptr<OA::Activity::InterUseful> icfgUseful;
    // icfgUseful = usefulman->performAnalysis(icfg, parambind, alias,
    //                                        OA::DataFlow::ITERATIVE);
    // icfgUseful->output(*irInterface);


    //! ====================================
    //! ICFGVaryActive
    //! ====================================

    // OA::OA_ptr<OA::Activity::ManagerICFGVaryActive> varyman;
    // varyman = new OA::Activity::ManagerICFGVaryActive(irInterface);
    // OA::OA_ptr<OA::Activity::ActivePerStmt> inActive;
    // inActive = varyman->performAnalysis(icfg, alias,
    //                            icfgDep, icfgUseful, OA::DataFlow::ITERATIVE);
    // inActive->output(*irInterface);


    //! ====================================
    //! ICFGActivity
    //! ====================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    OA::OA_ptr<OA::Activity::ManagerICFGActive> activeman;
    activeman = new OA::Activity::ManagerICFGActive(irInterface);
    OA::OA_ptr<OA::Activity::InterActiveFortran> active;
    active = activeman->performAnalysis(icfg, parambind,
                                        alias, OA::DataFlow::ITERATIVE);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf ICFGActivity seconds elapsed\n", t2-t1);

    active->output(*irInterface,*alias);

}



//! ================================
//! Sac07 ICFGActivity
//! ===============================


static int
TestIR_OAsac07ICFGActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{
    // FIXME: put timer usage in debug statements and have debug be
    // set with command line flag
    TIMER mytimer;

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //! =================================
    //! Control Flow Graph Timings
    //! ================================

    timer_start(&mytimer);

    //! eachCFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    timer_end(&mytimer);
    printf("%6lf CFG seconds elapsed\n", timer_numsecs(&mytimer));


    //! ================================
    //! AliasTagFIAlias Timings
    //! ================================


    timer_start(&mytimer);

    //! FIAliasAliasTag
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);

    timer_end(&mytimer);
    printf("%6lf FIAliasAliasTag seconds elapsed\n", timer_numsecs(&mytimer));


    //! =================================
    //! CallGraph Timings
    //! =================================

    timer_start(&mytimer);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    timer_end(&mytimer);
    printf("%6lf CallGraph seconds elapsed\n", timer_numsecs(&mytimer));


    //! =====================================
    //! ParamBindings
    //! =====================================


    // FIXME: are these still being used?
    timer_start(&mytimer);

    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);

    timer_end(&mytimer);
    printf("%6lf ParamBindings seconds elapsed\n", timer_numsecs(&mytimer));

    //! =====================================
    //! ICFG
    //! =====================================

    timer_start(&mytimer);

    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);

    timer_end(&mytimer);
    printf("%6lf ICFG seconds elapsed\n", timer_numsecs(&mytimer));


    //! ====================================
    //! ICFGActivity
    //! ====================================

    timer_start(&mytimer);

    OA::OA_ptr<OA::Activity::ManagerICFGActive> activeman;
    activeman = new OA::Activity::ManagerICFGActive(irInterface);
    OA::OA_ptr<OA::Activity::InterActiveFortran> active;
    active = activeman->performAnalysis(icfg, parambind,
                                        alias, OA::DataFlow::ITERATIVE);

    timer_end(&mytimer);
    printf("%6lf ICFGActivity seconds elapsed\n", timer_numsecs(&mytimer));

    active->output(*irInterface,*alias);

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


    // ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);


    // ICFGReachConsts
    OA::OA_ptr<OA::ReachConsts::ManagerICFGReachConsts> ircsman;
    ircsman = new OA::ReachConsts::ManagerICFGReachConsts(irInterface);
    OA::OA_ptr<OA::ReachConsts::InterReachConsts> ircs
        = ircsman->performAnalysis(icfg, alias, OA::DataFlow::ITERATIVE);
 
    ircs->output(*irInterface,*alias);

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

/*
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

*/
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

/*
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

*/
    return 0;
}







//! ===== Context Sensitive and Flow Insensitive Activity Analysis =====

static int
TestIR_OACSFIActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

    Diag_Set_Phase("WHIRL tester: TestIR_OACSFIActivity (Context-Sensitive and Flow-Insensitive)");
/*

    //! CFG
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


    //! call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);


    //! ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);


    //! Context-Sensitive and Flow-inSensitive Activity Analysis
    //! Def-Use Graph
    OA::OA_ptr<OA::DUG::ManagerDUGStandard> dugman;
    dugman = new OA::DUG::ManagerDUGStandard(irInterface, irInterface);
    OA::OA_ptr<OA::DUG::DUGStandard> dug
        = dugman->performAnalysis(procIter, parambind, cgraph);

    dugman->transitiveClosureDepMatrix(cgraph);
//    dug->dumpdot(cout, irInterface);
//    dug->output(*irInterface);

    // Def-Use Activity Analysis
    OA::OA_ptr<OA::Activity::ManagerDUActive> duactiveman;
    duactiveman = new OA::Activity::ManagerDUActive(irInterface, dug);
    OA::OA_ptr<OA::Activity::InterActiveFortran> duactive;
    duactive = duactiveman->performAnalysis( parambind);

    //duactive->dump(cout, irInterface);
    duactive->output(*irInterface, *alias);

*/
    return 0;

}

static int
TestIR_OACSFIAliasAliasTag(std::ostream& os, PU_Info* pu_forest,
                           OA::OA_ptr<Open64IRInterface> irInterface,
                           int ccmax)
{

    Diag_Set_Phase("WHIRL tester: TestIR_OACSFIAliasAliasTag (Context-Sensitive Flow-Insensitive)");

    /*

    //! CFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);
    */

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);


    // need FIAlias to make call graph

    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);


    //! call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);


    /*
    //! ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);
    */


    //! CallContexts
    
    // set CallContext k-level to ccmax
    OA::Alias::CallContext::setMaxDegree((unsigned int) ccmax);
    std::cout << "CallContext::setMaxDegree(" << ccmax << ")\n\n";
    
    OA::OA_ptr<OA::Alias::ManagerCallContexts> ccman;
    ccman = new OA::Alias::ManagerCallContexts(irInterface);
    OA::OA_ptr<OA::Alias::CCSetPerProc> ccResults;
    ccResults = ccman->performAnalysis(cgraph);

    //! CSFIAliasAliasTag

    OA::OA_ptr<OA::Alias::ManagerCSFIAliasAliasTag> csfialiasman;
    csfialiasman = new OA::Alias::ManagerCSFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> csfialias;
    csfialias = csfialiasman->performAnalysis(cgraph,ccResults);
    
    csfialias->output(*irInterface);

    return 0;
}

//! ===============================================================
//! ICFGReachConst
//! ===============================================================

 
static int
TestIR_OAICFGCSReachConsts(std::ostream& os, PU_Info* pu_forest,
                           OA::OA_ptr<Open64IRInterface> irInterface,
                           int ccmax)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAICFGCSReachConsts");

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
    //interAlias = new OA::Alias::InterAliasResults(alias);

    //! call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    //cgraph->dump(std::cout, irInterface);

    //! CallContexts
    
    // set CallContext k-level to ccmax
    OA::Alias::CallContext::setMaxDegree((unsigned int) ccmax);
    std::cout << "CallContext::setMaxDegree(" << ccmax << ")\n\n";
    
    OA::OA_ptr<OA::Alias::ManagerCallContexts> ccman;
    ccman = new OA::Alias::ManagerCallContexts(irInterface);
    OA::OA_ptr<OA::Alias::CCSetPerProc> ccResults;
    ccResults = ccman->performAnalysis(cgraph);

    //! CSFIAliasAliasTag

    OA::OA_ptr<OA::Alias::ManagerCSFIAliasAliasTag> csfialiasman;
    csfialiasman = new OA::Alias::ManagerCSFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> csfialias;
    csfialias = csfialiasman->performAnalysis(cgraph,ccResults);
    //interAlias = new OA::Alias::InterAliasResults(csfialias);
    
    // csfialias->output(*irInterface);

    //! ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);


    //! ICFGCSReachConsts
    OA::OA_ptr<OA::ReachConsts::ManagerICFGCSReachConsts> iCSrcsman;
    iCSrcsman = new OA::ReachConsts::ManagerICFGCSReachConsts(irInterface);
    OA::OA_ptr<OA::ReachConsts::InterReachConsts> iCSrcs
      = iCSrcsman->performAnalysis(icfg, csfialias, ccResults, 
                                   OA::DataFlow::ITERATIVE);
 
    iCSrcs->output(*irInterface,*csfialias);

    return 0;
}


//! ==============================================================
//! ICFGCSActivity
//! ==============================================================


static int
TestIR_OAICFGCSActivity(std::ostream& os, PU_Info* pu_forest,
                        OA::OA_ptr<Open64IRInterface> irInterface,
                        int ccmax)
{
    //timeval tim;
    //double t1,t2;

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //! =================================
    //! Control Flow Graph Timings
    //! ================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    //! eachCFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf CFG seconds elapsed\n", t2-t1);


    //! ================================
    //! AliasTagFIAlias Timings
    //! ================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    //! FIAliasAliasMap
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasTag> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias = fialiasman->performAnalysis(procIter);

    //alias->output(*irInterface);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf AliasTagFIAlias seconds elapsed\n", t2-t1);

    
    //! =================================
    //! CallGraph Timings
    //! =================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, alias);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf CallGraph seconds elapsed\n", t2-t1);


    //! CallContexts
    
    // set CallContext k-level to ccmax
    OA::Alias::CallContext::setMaxDegree((unsigned int) ccmax);
    std::cout << "CallContext::setMaxDegree(" << ccmax << "\n\n";
    
    OA::OA_ptr<OA::Alias::ManagerCallContexts> ccman;
    ccman = new OA::Alias::ManagerCallContexts(irInterface);
    OA::OA_ptr<OA::Alias::CCSetPerProc> ccResults;
    ccResults = ccman->performAnalysis(cgraph);




    //! ================================
    //! AliasTagCSFIAlias Timings
    //! ================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    //! CSFIAliasAliasMap

    OA::OA_ptr<OA::Alias::ManagerCSFIAliasAliasTag> csfialiasman;
    csfialiasman = new OA::Alias::ManagerCSFIAliasAliasTag(irInterface);
    OA::OA_ptr<OA::Alias::Interface> csfialias;
    csfialias = csfialiasman->performAnalysis(cgraph, ccResults);
    
    //csfialias->output(*irInterface);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf AliasTagCSFIAlias seconds elapsed\n", t2-t1);

    
    //! =====================================
    //! ICFG
    //! =====================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf ICFG seconds elapsed\n", t2-t1);

    //! =====================================
    //! ICFGCSUseful   (for testing)
    //! =====================================

    //OA::OA_ptr<OA::Activity::ManagerICFGCSUseful> csusefulman;
    //csusefulman = new OA::Activity::ManagerICFGCSUseful(irInterface);
    //OA::OA_ptr<OA::Activity::InterUseful> csInterUseful;
    //csInterUseful = csusefulman->performAnalysis(icfg, csfialias, ccResults,
    //                                        OA::DataFlow::ITERATIVE);
    //csInterUseful->output(*irInterface);


    //! ====================================
    //! ICFGCSVaryActive
    //! ====================================

    //OA::OA_ptr<OA::Activity::ManagerICFGCSVaryActive> csvaryman;
    //csvaryman = new OA::Activity::ManagerICFGCSVaryActive(irInterface);
    //OA::OA_ptr<OA::Activity::ActivePerStmt> CSInActive;
    //CSInActive = csvaryman->performAnalysis(icfg, csfialias, ccResults,
    //                                      csInterUseful,
    //                                      OA::DataFlow::ITERATIVE);
    //CSInActive->output(*irInterface);


    //! ====================================
    //! ICFGCSActivity
    //! ====================================

    //gettimeofday(&tim, NULL);
    //t1 = tim.tv_sec+(tim.tv_usec/1000000.0);

    OA::OA_ptr<OA::Activity::ManagerICFGCSActive> csactiveman;
    csactiveman = new OA::Activity::ManagerICFGCSActive(irInterface);
    OA::OA_ptr<OA::Activity::InterActiveFortran> csactive;
    csactive = csactiveman->performAnalysis(icfg, csfialias, ccResults,
                                            OA::DataFlow::ITERATIVE);

    //gettimeofday(&tim, NULL);
    //t2 = tim.tv_sec+(tim.tv_usec/1000000.0);
    //printf("%6lf ICFGActivity seconds elapsed\n", t2-t1);

    csactive->output(*irInterface,*alias);

}


