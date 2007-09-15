// -*-Mode: C++;-*-
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

#include <OpenAnalysis/CallGraph/ManagerCallGraph.hpp>
#include <OpenAnalysis/CFG/ManagerCFG.hpp>
#include <OpenAnalysis/Alias/ManagerAliasMapBasic.hpp>
#include <OpenAnalysis/Alias/ManagerInterAliasMapBasic.hpp>
#include <OpenAnalysis/Alias/ManagerFIAliasAliasMap.hpp>
//#include <OpenAnalysis/Alias/ManagerNoAddressOf.hpp>
#include <OpenAnalysis/ReachDefs/ManagerReachDefsStandard.hpp>
#include <OpenAnalysis/SideEffect/ManagerSideEffectStandard.hpp>
#include <OpenAnalysis/SideEffect/InterSideEffectStandard.hpp>
#include <OpenAnalysis/SideEffect/ManagerInterSideEffectStandard.hpp>
#include <OpenAnalysis/UDDUChains/ManagerUDDUChainsStandard.hpp>
#include <OpenAnalysis/XAIF/UDDUChainsXAIF.hpp>
#include <OpenAnalysis/XAIF/ManagerUDDUChainsXAIF.hpp>
#include <OpenAnalysis/ReachConsts/ManagerReachConstsStandard.hpp>
#include <OpenAnalysis/ReachConsts/ManagerICFGReachConsts.hpp>

#include <OpenAnalysis/XAIF/ManagerAliasMapXAIF.hpp>

#include <OpenAnalysis/Activity/ManagerActiveStandard.hpp>
#include <OpenAnalysis/Activity/ManagerEachActive.hpp>
//#include <OpenAnalysis/Activity/ManagerInterActive.hpp>
#include <OpenAnalysis/CFG/EachCFGStandard.hpp>
#include <OpenAnalysis/DataFlow/ManagerParamBindings.hpp>
#include <OpenAnalysis/Activity/ManagerInterDep.hpp>
#include <OpenAnalysis/ICFG/ManagerICFG.hpp>
#include <OpenAnalysis/Activity/ManagerICFGActive.hpp>
#include <OpenAnalysis/Activity/ManagerICFGDep.hpp>
#include <OpenAnalysis/Linearity/ManagerLinearityStandard.hpp>
#include <OpenAnalysis/Activity/ManagerICFGUseful.hpp>
#include <OpenAnalysis/Activity/ManagerICFGVaryActive.hpp>

#include <OpenAnalysis/Utils/OutputBuilderDOT.hpp>


/*! Header files for Context-Sensitive and Flow-inSensitive Activity Analysis */
#include <OpenAnalysis/CSFIActivity/ManagerDUGStandard.hpp>
#include <OpenAnalysis/CSFIActivity/ManagerDUActive.hpp>



#include <OpenAnalysis/Activity/InterActiveFortran.hpp>

//#include <OpenAnalysis/Utils/SCC.hpp>

//************************** Forward Declarations ***************************

static int
FindMemRefExprInEachPU(std::ostream& os, PU_Info* pu_forest);

static int
FindMemRefExprInPU(std::ostream& os, PU_Info* pu_forest);

static int
TestIR_OA(std::ostream& os, PU_Info* pu_forest, int runMode );

static int
TestIR_OACallGraph(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);
static int
TestIR_OAParamBindings(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAInterSideEffect(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAInterDep(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAEachActivity(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);


static int
TestIR_OACSFIActivity(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAICFG(std::ostream& os, PU_Info* pu_forest,
              OA::OA_ptr<Open64IRInterface> irInterface,
              std::string& dotFileNm);

static int
TestIR_OAICFGReachConsts(std::ostream& os, PU_Info* pu_forest,
              OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAICFGDep(std::ostream& os, PU_Info* pu_forest,
                 OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAICFGActivity(std::ostream& os, PU_Info* pu_forest,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OACommonBlockVars(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAliasMapXAIFFIAlias(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAliasMapFIAlias(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAliasMapXAIFInter(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAliasMapInter(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAssignPair(std::ostream& os, PU_Info* pu_forest,
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

/*
  const char* cmd = argv[0];
  const char* whirlFileNm = argv[1]; // better be the WHIRL file or else
  if (!argv[1]) {
    std::cerr << "usage: " << cmd << " <whirl_file>" << std::endl;
    exit(1);
  }
*/
  
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
  
  switch (args.runMode)
  {
    case 2:
      // MemRefExpr
      FindMemRefExprInEachPU(std::cout, pu_forest);
      break;
    case 5: // CallGraph
      TestIR_OACallGraph(std::cout, pu_forest, irInterface);
      break;
    case 13: // InterSideEffect
      TestIR_OAInterSideEffect(std::cout, pu_forest, irInterface);
      break;
    case 15: // EachActivity
      TestIR_OAEachActivity(std::cout, pu_forest, irInterface);
      break;
    case 16: // ParamBindings
      TestIR_OAParamBindings(std::cout, pu_forest, irInterface);
      break;
    case 18: // InterDep
      TestIR_OAInterDep(std::cout, pu_forest, irInterface);
      break;
    case 20: // ICFG
      {
        std::string dotFileNm = args.whirlFileNm;
        TestIR_OAICFG(std::cout, pu_forest, irInterface,dotFileNm);
      }
      break;
    case 21: // ICFGActivity
      TestIR_OAICFGActivity(std::cout, pu_forest, irInterface);
      break;
    case 22: // CommonBlock
      TestIR_OACommonBlockVars(std::cout, pu_forest, irInterface);
      break;
    //case 24: // AliasMapInter, requires isRefParam and is wrong anyway
      //TestIR_OAAliasMapInter(std::cout, pu_forest, irInterface);
      //break;
    //case 26: // AliasMapXAIFInter
      //TestIR_OAAliasMapXAIFInter(std::cout, pu_forest, irInterface);
      //break;
    case 27: // AliasMapFIAlias
      TestIR_OAAliasMapFIAlias(std::cout, pu_forest, irInterface);
      break;
    case 28: // AliasMapXAIFFIAlias
      TestIR_OAAliasMapXAIFFIAlias(std::cout, pu_forest, irInterface);
      break;
    case 29: // ICFGDep
      TestIR_OAICFGDep(std::cout, pu_forest, irInterface);
      break;
    case 31: // ICFGReachConsts
      TestIR_OAICFGReachConsts(std::cout, pu_forest, irInterface);
      break;
    case 32: // CSFIActivity
      TestIR_OACSFIActivity(std::cout, pu_forest, irInterface);
      break;
    case 1: // CFG
    //case 3: // Alias
    //case 4: // AliasMap
    case 6: // ReachDefs
    case 7: // UDDUChains
    case 8: // UDDUChainsXAIF
    case 9: // MPICFG
    case 10: // ReachConsts
      
    //case 11: // AliasMapXAIF
    case 12: // SideEffect
    case 14: // Activity
    case 17: // SymAliasSets bottom, deprecated
    case 23: // AliasMapBasic
    case 25: // AliasMapXAIFBasic
    case 30: // Linearity
      TestIR_OA(std::cout, pu_forest, args.runMode );
      break;
    case 33: // AssignPair
      TestIR_OAAssignPair(std::cout, pu_forest, irInterface);
      break;
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


//***************************************************************************
// 
//***************************************************************************

static int
FindMemRefExprInEachPU(std::ostream& os, PU_Info* pu_forest)
{
  Open64IRProcIterator procIt(pu_forest);
  for ( ; procIt.isValid(); ++procIt) { 
    
    // The PU_Info* for this PU
    PU_Info* pu = (PU_Info*)procIt.current().hval();

    // The root of the WHIRL tree
    //WN* wn_pu = PU_Info_tree_ptr(pu);
    
    FindMemRefExprInPU(os, pu);
  }
  return 0;
}

static int
FindMemRefExprInPU(std::ostream& os, PU_Info* pu)
{
  OA::OA_ptr<Open64IRInterface> ir; ir = new Open64IRInterface();
  OA::ProcHandle proc((OA::irhandle_t)pu);
  
  // for each statement stmt
  //   for each memory reference in stmt
  //     for each mem-ref-expr for the memory reference
  //        output information about the memory reference expressions
  OA::OA_ptr<OA::IRStmtIterator> sIt = ir->getStmtIterator(proc);
  for ( ; sIt->isValid(); (*sIt)++) {
    OA::StmtHandle stmt = sIt->current();
    
    std::cout << "============================================" << std::endl;
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
  return 0;
}

static int
TestIR_OACFG_ForEachWNPU(std::ostream& os, PU_Info* pu,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAlias_ForEachWNPU(std::ostream& os, PU_Info* pu,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAliasMapBasic_ForEachWNPU(std::ostream& os, PU_Info* pu,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAAliasMapXAIFBasic_ForEachWNPU(std::ostream& os, PU_Info* pu,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAReachDefs(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect);

static int
TestIR_OALinearity(std::ostream& os, PU_Info* pu,
                       OA::OA_ptr<Open64IRInterface> irInterface,
                       OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
                       OA::OA_ptr<OA::DataFlow::ParamBindings> parambind);
/*static int
TestIR_OAUDDUChains(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect);*/

static int
TestIR_OAUDDUChains(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect);


static int
TestIR_OAUDDUChainsXAIF(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect);


static int
TestIR_OAExprTree(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAReachConsts(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect);

static int
TestIR_OASideEffect_ForEachWNPU(std::ostream& os, PU_Info* pu,
                    OA::OA_ptr<Open64IRInterface> irInterface);

static int
TestIR_OAActivity_ForEachWNPU(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect);


static int
TestIR_OA(std::ostream& os, PU_Info* pu_forest, int runMode )
{
  
  if (!pu_forest) { return 0; }

  OA::OA_ptr<Open64IRInterface> irInterface; irInterface = new Open64IRInterface();
  
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  // Alias 
//  OA::OA_ptr<OA::Alias::ManagerInterAliasMapBasic> interaliasmapman;
//  interaliasmapman = new OA::Alias::ManagerInterAliasMapBasic(irInterface);
//  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
//  interAlias = interaliasmapman->performAnalysis();

  //FIAlias
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = fialiasman->performAnalysis(procIter);

  // call graph
  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter, interAlias);

  //cgraph->dump(std::cout, irInterface);

  // ParamBindings
  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> parambindman;
  parambindman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind 
      = parambindman->performAnalysis(cgraph);

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

  //======================================== loop over procedures
  Open64IRProcIterator procIt(pu_forest);
  procIt.reset();
  for ( ; procIt.isValid(); ++procIt) { 
    
    // The PU_Info* for this PU
    PU_Info* pu = (PU_Info*)procIt.current().hval();

    switch (runMode) {
      case 1:
        TestIR_OACFG_ForEachWNPU(os, pu, irInterface);
        break;
      //case 3: // Alias
      //  TestIR_OAAlias_ForEachWNPU(os, pu, irInterface);
      //  break;
      //case 4: // AliasMaps
      //  TestIR_OAAliasMap_ForEachWNPU(os, pu, irInterface);
      //  break;
      case 6:
        TestIR_OAReachDefs(os, pu, irInterface, interAlias, interSE);
        break;
      case 7:
        TestIR_OAUDDUChains(os, pu, irInterface, interAlias, interSE);
        break;
      case 8:
        TestIR_OAUDDUChainsXAIF(os, pu, irInterface, interAlias, interSE);
        break;
      case 9:
        TestIR_OAExprTree(os, pu, irInterface);
        break;
      case 10:
        TestIR_OAReachConsts(os, pu, irInterface, interSE);
        break;
      //case 11: // AliasMapXAIF
      //  TestIR_OAAliasMapXAIF_ForEachWNPU(os, pu, irInterface);
      //  break;
      case 12: // SideEffect
        //TestIR_OASideEffect_ForEachWNPU(os, pu, irInterface);
        break;
      case 14: // Activity
        //TestIR_OAActivity_ForEachWNPU(os, pu, irInterface, interSE);
        break;
      case 17: // SymAliasSets bottom, deprecated
        break;
      case 23: // AliasMapBasic
        TestIR_OAAliasMapBasic_ForEachWNPU(os, pu, irInterface);
        break;
      case 25: // AliasMapXAIFBasic
        TestIR_OAAliasMapXAIFBasic_ForEachWNPU(os, pu, irInterface);
        break;
      case 30: // Linearity
        TestIR_OALinearity(os, pu, irInterface, interAlias, parambind);
        break;
    }
  }
  return 0;
}

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

  //std::cout << "SCC sets" << std::endl;
  //OA::OA_ptr<OA::SCCSet> sccset;
  //sccset = new OA::SCCSet(cfg);
  //sccset->dump(std::cout);
  
  return 0;
}

static int
TestIR_OACallGraph(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OACallGraph");

  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);
  
  //FIAlias
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = fialiasman->performAnalysis(procIter);

  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter,interAlias);

  // text output
  cgraph->output(*irInterface);
  // dot output
  OA::OA_ptr<OA::OutputBuilder> outBuild;
  outBuild = new OA::OutputBuilderDOT;
  cgraph->configOutput(outBuild);
  cgraph->output(*irInterface);

  return 0;
}

static int
TestIR_OAParamBindings(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAParamBindings");

  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //FIAlias
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = fialiasman->performAnalysis(procIter);

      // CallGraph
  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter, interAlias);

  //cgraph->dump(std::cout, irInterface);

  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
  pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
  parambind = pbman->performAnalysis(cgraph);
  parambind->output(*irInterface);

  return 0;
}

static int
TestIR_OAInterDep(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAInterDep");
/*
  // iterate over all functions and call kludge
  Open64IRProcIterator procIt(pu_forest);  
  
  // call graph
  OA::OA_ptr<OA::CallGraph::ManagerStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerStandard(irInterface);
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);
  OA::OA_ptr<OA::CallGraph::CallGraphStandard> cgraph = 
      cgraphman->performAnalysis(procIter);

  cgraph->dump(std::cout, irInterface);

  // ParamBindings
  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> parambindman;
  parambindman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind 
      = parambindman->performAnalysis(cgraph);

  parambind->dump(std::cout,irInterface);

  // Alias 
  OA::OA_ptr<OA::Alias::ManagerInsNoPtrInterAliasMap> interaliasmapman;
  interaliasmapman = new OA::Alias::ManagerInsNoPtrInterAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = interaliasmapman->performAnalysis(cgraph,parambind);
  interAlias->dump(std::cout,irInterface);
  
  // CFG 
  OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
  OA::OA_ptr<OA::CFG::ManagerStandard> cfgman;
  cfgman = new OA::CFG::ManagerStandard(irInterface);
  eachCFG = new OA::CFG::EachCFGStandard(cfgman);
  
  // Intra Side-Effect
  OA::OA_ptr<OA::SideEffect::ManagerStandard> sideeffectman;
  sideeffectman = new OA::SideEffect::ManagerStandard(irInterface);

  // InterSideEffect
  OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
  interSEman = new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);

  OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
  interSE = interSEman->performAnalysis(cgraph, parambind,
                                        interAlias, sideeffectman);

  interSE->dump(std::cout, irInterface);


  // InterDep
  OA::OA_ptr<OA::Activity::ManagerInterDep> interDepMan;
  interDepMan = new OA::Activity::ManagerInterDep(irInterface);

  OA::OA_ptr<OA::Activity::InterDep> interDep;
  interDep = interDepMan->performAnalysis(cgraph, parambind, interAlias,
                                          interSE, eachCFG);

  interDep->dump(std::cout, irInterface);
*/
  return 0;
}



static int
TestIR_OAInterSideEffect(std::ostream& os, PU_Info* pu_forest,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAInterSideEffect");

  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //FIAlias
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = fialiasman->performAnalysis(procIter);

  // CallGraph
  OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
  OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter, interAlias);

  //cgraph->dump(std::cout, irInterface);

  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
  pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
  parambind = pbman->performAnalysis(cgraph);

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

  //interSE->dump(std::cout, irInterface);
  interSE->output(*irInterface);

  return 0;
}

static int
TestIR_OAICFG(std::ostream& os, PU_Info* pu_forest,
                        OA::OA_ptr<Open64IRInterface> irInterface,
                        std::string& dotFileNm)
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

    //FIAlias
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
    OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
    interAlias = fialiasman->performAnalysis(procIter);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter, interAlias);

    //cgraph->dump(std::cout, irInterface);

    // ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);

    //std::cout << "%%%%%%%% ICFG constructed from input program" << std::endl;
    //icfg->dump(std::cout, irInterface);
    //icfg->dumpdot(std::cout, irInterface);
    
    // text output
    icfg->output(*irInterface);
    // dot output
    OA::OA_ptr<OA::OutputBuilder> outBuild;
    outBuild = new OA::OutputBuilderDOT;
    icfg->configOutput(outBuild);
    icfg->output(*irInterface);


    // debugging by BK
    {
      //icfg->output(*irInterface);
      
      //{
      //  std::cout << "%%%%%%%%%%% second ICFG constructed from input program"
      //            << std::endl;
      //}

      // dot files are saved in same path as application files
      // this should not interfere with regression testing
      std::string icfgDotFileNm = dotFileNm;
      icfgDotFileNm.append(".ICFG.dot");
      std::ofstream dotFile;
      dotFile.open(icfgDotFileNm.c_str());
      if (!dotFile.fail()) {
        icfg->dumpdot(dotFile, irInterface);
      } else {
        icfg->dumpdot(std::cout, irInterface);
        cout << "\n\n could not open '" << icfgDotFileNm << "'\n\n";
      }
      dotFile.close();
    
    }

    return 0;
}

static int
TestIR_OAICFGDep(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAICFGDep");

    // eachCFG 
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);
    
    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //FIAlias
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
    OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
    interAlias = fialiasman->performAnalysis(procIter);

    //interAlias->output(*irInterface);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter, interAlias);

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

    //ICFGDep
    OA::OA_ptr<OA::Activity::ManagerICFGDep> icfgdepman;
    icfgdepman = new OA::Activity::ManagerICFGDep(irInterface);
    OA::OA_ptr<OA::Activity::ICFGDep> icfgDep;
    icfgDep = icfgdepman->performAnalysis(icfg, parambind, interAlias,
                                          OA::DataFlow::ITERATIVE);

    // text output
    OA::OA_ptr<OA::OutputBuilder> outBuild;

    /*
    outBuild = new OA::OutputBuilderText;
    icfg->configOutput(outBuild);
    icfg->output(*irInterface);

    outBuild = new OA::OutputBuilderDOT;
    icfg->configOutput(outBuild);
    icfg->output(*irInterface);
    
    outBuild = new OA::OutputBuilderText;
    icfgDep->configOutput(outBuild);
    */

    icfgDep->output(*irInterface);

    // dump output
    //icfgDep->dump(std::cout,irInterface);

    return 0;
}

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

    //FIAlias
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
    OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
    interAlias = fialiasman->performAnalysis(procIter);

    //interAlias->output(*irInterface);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter, interAlias);

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

    // Intra Side-Effect
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> sideeffectman;
    sideeffectman = new OA::SideEffect::ManagerSideEffectStandard(irInterface);
    
    // InterSideEffect
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
    interSEman = new 
      OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
    
    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
    interSE = interSEman->performAnalysis(cgraph, parambind,
                                        interAlias, sideeffectman,
                                        OA::DataFlow::ITERATIVE);
    
    // ICFGReachConsts
    OA::OA_ptr<OA::ReachConsts::ManagerICFGReachConsts> ircsman;
    ircsman = new OA::ReachConsts::ManagerICFGReachConsts(irInterface);
    OA::OA_ptr<OA::ReachConsts::InterReachConsts> ircs
      = ircsman->performAnalysis(icfg,parambind,interAlias,interSE,
                                 OA::DataFlow::ITERATIVE);

    ircs->output(*irInterface);

    return 0;
}



static int
TestIR_OAAliasMapFIAlias(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAAliasMapFIAlias");
  
  // need a procedure iterator
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //FIAlias
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias
      = fialiasman->performAnalysis(procIter);
  interAlias->output(*irInterface);
  
  return 0;
}

/*
static int
TestIR_OAICFGActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAICFGActivity");

}
*/

static int
TestIR_OAAliasMapXAIFFIAlias(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{


  Diag_Set_Phase("WHIRL tester: TestIR_OAAliasMapXAIFFIAlias");

  // need a procedure iterator
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);

  //FIAlias
  OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
  fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasInterface> interAlias
      = fialiasman->performAnalysis(procIter);
  for(procIter->reset(); procIter->isValid(); ++(*procIter)) {
    OA::ProcHandle procHandle = procIter->current();
    OA::OA_ptr<OA::Alias::Interface> alias
        = interAlias->getAliasResults(procHandle);

    // XAIF AliasMap
    OA::OA_ptr<OA::XAIF::ManagerAliasMapXAIF> aliasmapxaifman;
    aliasmapxaifman = new OA::XAIF::ManagerAliasMapXAIF(irInterface);
    OA::OA_ptr<OA::XAIF::AliasMapXAIF> aliasMapXAIF = 
      aliasmapxaifman->performAnalysis(procHandle,alias);

    aliasMapXAIF->output(*irInterface);
  }

 
  return 0;
}

static int
TestIR_OAAssignPair(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAAssignPair");

  // need a procedure iterator
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);
  for(procIter->reset(); procIter->isValid(); ++(*procIter)) {
    OA::ProcHandle proc = procIter->current();
    OA::OA_ptr<OA::IRStmtIterator> sIt 
        = irInterface->getStmtIterator(proc);
    for ( ; sIt->isValid(); (*sIt)++) {
        OA::StmtHandle stmt = sIt->current();
        std::cout << "========================================================" << std::endl;
        std::cout << "\nstmt : ";
        std::cout << irInterface->toString(stmt) << std::endl;

        std::cout << "\nAssignPairs : " << std::endl << std::endl;
        OA::OA_ptr<OA::AssignPairIterator> espIterPtr
            = irInterface->getAssignPairIterator(stmt);
        for ( ; espIterPtr->isValid(); ++(*espIterPtr)) {
            // unbundle pair
            OA::MemRefHandle mref = espIterPtr->currentTarget();
            OA::ExprHandle expr = espIterPtr->currentSource();
            std::cout << "\tmref = " 
                      << irInterface->toString(mref) << ", ";
            std::cout << "\texpr = " 
                      << irInterface->toString(expr) << std::endl;
        }

        OA::OA_ptr<OA::ExprHandleIterator> exprIter;
        exprIter = irInterface->getExprHandleIterator(stmt);

        std::cout << "\nExprTree : " << std::endl;
        for ( ; exprIter->isValid(); (*exprIter)++) {
            OA::ExprHandle expr = exprIter->current();
            std::cout << "\t--expr----------------------------------------\n";
            std::cout << "\texpr = " << irInterface->toString(expr) << std::endl;
            std::cout << "\t----------------------------------------------";
            OA::OA_ptr<OA::ExprTree> eTreePtr = irInterface->getExprTree(expr);
            eTreePtr->output(*irInterface);
        }
    }
  }
  return 0;
}

static int
TestIR_OAAliasMapInter(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAAliasMapInter");
/* 
  // call graph
  OA::OA_ptr<OA::CallGraph::ManagerStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerStandard(irInterface);
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);
  OA::OA_ptr<OA::CallGraph::CallGraphStandard> cgraph = 
      cgraphman->performAnalysis(procIter);

  //cgraph->dump(std::cout, irInterface);

  // ParamBindings
  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> parambindman;
  parambindman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind 
      = parambindman->performAnalysis(cgraph);

  //parambind->dump(std::cout,irInterface);

  // Alias 
  OA::OA_ptr<OA::Alias::ManagerInsNoPtrInterAliasMap> interaliasmapman;
  interaliasmapman = new OA::Alias::ManagerInsNoPtrInterAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = interaliasmapman->performAnalysis(cgraph,parambind);

  //interAlias->dump(std::cout,irInterface);
  interAlias->output(*irInterface);
*/  
  return 0;
}

static int
TestIR_OAAliasMapXAIFInter(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAAliasMapXAIFInter");
 /* 
  // call graph
  OA::OA_ptr<OA::CallGraph::ManagerStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerStandard(irInterface);
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);
  OA::OA_ptr<OA::CallGraph::CallGraphStandard> cgraph = 
      cgraphman->performAnalysis(procIter);

  //cgraph->dump(std::cout, irInterface);

  // ParamBindings
  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> parambindman;
  parambindman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind 
      = parambindman->performAnalysis(cgraph);

  //parambind->dump(std::cout,irInterface);

  // Alias 
  OA::OA_ptr<OA::Alias::ManagerInsNoPtrInterAliasMap> interaliasmapman;
  interaliasmapman = new OA::Alias::ManagerInsNoPtrInterAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = interaliasmapman->performAnalysis(cgraph,parambind);

  //interAlias->dump(std::cout,irInterface);
//  interAlias->output(irInterface);
  Open64IRProcIterator procIt(pu_forest);
  procIt.reset();
  for ( ; procIt.isValid(); ++procIt) { 
    
    // The PU_Info* for this PU
    PU_Info* pu = (PU_Info*)procIt.current().hval();

    OA::OA_ptr<OA::Alias::Interface> aliasMap 
        = interAlias->getAliasResults((OA::irhandle_t)pu);
 
    // XAIF AliasMap
    OA::OA_ptr<OA::XAIF::ManagerAliasMapXAIF> aliasmapxaifman;
    aliasmapxaifman = new OA::XAIF::ManagerAliasMapXAIF(irInterface);
    OA::OA_ptr<OA::XAIF::AliasMapXAIF> aliasMapXAIF = 
      aliasmapxaifman->performAnalysis((OA::irhandle_t)pu,aliasMap);

    aliasMapXAIF->output(*irInterface);
  }
*/ 
  return 0;
}


static int
TestIR_OAICFGActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAICFGActivity");

    // eachCFG 
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);
    
    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //FIAlias
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
    OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
    interAlias = fialiasman->performAnalysis(procIter);

    //interAlias->output(*irInterface);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph = 
      cgraphman->performAnalysis(procIter, interAlias);

    //cgraph->dump(std::cout, irInterface);

    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);

    // Intra Side-Effect
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> sideeffectman;
    sideeffectman = new OA::SideEffect::ManagerSideEffectStandard(irInterface);
    
    // InterSideEffect
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
    interSEman = new 
      OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
    
    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
    interSE = interSEman->performAnalysis(cgraph, parambind,
                                        interAlias, sideeffectman,
                                        OA::DataFlow::ITERATIVE);
    
    // ICFG
    OA::OA_ptr<OA::ICFG::ManagerICFGStandard> icfgman;
    icfgman = new OA::ICFG::ManagerICFGStandard(irInterface);
    OA::OA_ptr<OA::ICFG::ICFG> icfg;
    icfg = icfgman->performAnalysis(procIter,eachCFG,cgraph);

    // ----------------- testing separate pieces

    //ICFGDep  (for testing)
    OA::OA_ptr<OA::Activity::ManagerICFGDep> icfgdepman;
    icfgdepman = new OA::Activity::ManagerICFGDep(irInterface);
    OA::OA_ptr<OA::Activity::ICFGDep> icfgDep;
    icfgDep = icfgdepman->performAnalysis(icfg, parambind, interAlias,
                                          OA::DataFlow::ITERATIVE);

    //icfgDep->output(*irInterface);

    /*
    // ICFGUseful   (for testing)
    OA::OA_ptr<OA::Activity::ManagerICFGUseful> usefulman;
    usefulman = new OA::Activity::ManagerICFGUseful(irInterface);
    OA::OA_ptr<OA::Activity::InterUseful> icfgUseful;
    icfgUseful = usefulman->performAnalysis(icfg, parambind, interAlias, 
                                            interSE, icfgDep, OA::DataFlow::ITERATIVE);
    std::cout << "printing ICFGUseful" << std::endl;
    icfgUseful->output(*irInterface);
    
    //ICFGVaryActive    (for testing)
    OA::OA_ptr<OA::Activity::ManagerICFGVaryActive> varyman;
    varyman = new OA::Activity::ManagerICFGVaryActive(irInterface);
    OA::OA_ptr<OA::Activity::ActivePerStmt> inActive;
    inActive = varyman->performAnalysis(icfg, parambind,
                                        interAlias, icfgDep, icfgUseful,
                                        OA::DataFlow::ITERATIVE);
    std::cout << "Printing ICFGVaryActive" << std::endl;
    inActive->output(*irInterface);
    */

    // ----------------- Activity does the testing pieces above
    // ICFGActive
    OA::OA_ptr<OA::Activity::ManagerICFGActive> activeman;
    activeman = new OA::Activity::ManagerICFGActive(irInterface);
    OA::OA_ptr<OA::Activity::InterActive> active;
    active = activeman->performAnalysis(icfg, parambind,
                                        interAlias, interSE,
                                        OA::DataFlow::ITERATIVE);
   
    active->output(*irInterface);
    
    int numIterDep = 1;
    int numIterActive = 1;
    int numTotal = numIterDep + active->getNumIterUseful() 
      + active->getNumIterVary() + numIterActive;
    std::cout << "\n\nTotal Iters: " << numTotal << std::endl;


  return 0;
}


static int
TestIR_OACSFIActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

    Diag_Set_Phase("WHIRL tester: TestIR_OAInterAActivity (Context-Sensitive and Flow-Insensitive)");

    // eachCFG
    OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgman;
    cfgman = new OA::CFG::ManagerCFGStandard(irInterface);
    eachCFG = new OA::CFG::EachCFGStandard(cfgman);

    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu_forest);

    //FIAlias
    OA::OA_ptr<OA::Alias::ManagerFIAliasAliasMap> fialiasman;
    fialiasman= new OA::Alias::ManagerFIAliasAliasMap(irInterface);
    OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
    interAlias = fialiasman->performAnalysis(procIter);

    //interAlias->output(*irInterface);

    // call graph
    OA::OA_ptr<OA::CallGraph::ManagerCallGraphStandard> cgraphman;
    cgraphman = new OA::CallGraph::ManagerCallGraphStandard(irInterface);
    OA::OA_ptr<OA::CallGraph::CallGraph> cgraph =
      cgraphman->performAnalysis(procIter, interAlias);

    //cgraph->dump(std::cout, irInterface);

    //ParamBindings
    OA::OA_ptr<OA::DataFlow::ManagerParamBindings> pbman;
    pbman = new OA::DataFlow::ManagerParamBindings(irInterface);
    OA::OA_ptr<OA::DataFlow::ParamBindings> parambind;
    parambind = pbman->performAnalysis(cgraph);

    // Intra Side-Effect
    OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> sideeffectman;
    sideeffectman = new OA::SideEffect::ManagerSideEffectStandard(irInterface);

    // InterSideEffect
    OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
    interSEman = new
      OA::SideEffect::ManagerInterSideEffectStandard(irInterface);

    OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
    interSE = interSEman->performAnalysis(cgraph, parambind,
                                        interAlias, sideeffectman,
                                        OA::DataFlow::ITERATIVE);

    // Context-Sensitive and Flow-inSensitive Activity Analysis
    // Def-Use Graph
    OA::OA_ptr<OA::DUG::ManagerDUGStandard> dugman;
    dugman = new OA::DUG::ManagerDUGStandard(irInterface, irInterface);
    OA::OA_ptr<OA::DUG::DUGStandard> dug
        = dugman->performAnalysis(procIter, parambind, cgraph);
        
    dugman->transitiveClosureDepMatrix(cgraph);
    //dug->dumpdot(cout, irInterface);
    // dug->output(*irInterface);

    // Def-Use Activity Analysis
    OA::OA_ptr<OA::Activity::ManagerDUActive> duactiveman;
    duactiveman = new OA::Activity::ManagerDUActive(irInterface, dug);
    OA::OA_ptr<OA::Activity::InterActiveFortran> duactive;
    duactive = duactiveman->performAnalysis( parambind);

    //duactive->dump(cout, irInterface);
    duactive->output(*irInterface);

}


/*! Michelle's Activity Analysis. 
 * Commented out by PLM 100106 and replaced temporarily by Jaewook's 
 * Activity Analysis Driver.

static int
TestIR_OAInterActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{

  Diag_Set_Phase("WHIRL tester: TestIR_OAInterActivity");
  
  // call graph
  OA::OA_ptr<OA::CallGraph::ManagerStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerStandard(irInterface);
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);
  OA::OA_ptr<OA::CallGraph::CallGraphStandard> cgraph = 
      cgraphman->performAnalysis(procIter);

  cgraph->dump(std::cout, irInterface);

  // ParamBindings
  OA::OA_ptr<OA::DataFlow::ManagerParamBindings> parambindman;
  parambindman = new OA::DataFlow::ManagerParamBindings(irInterface);
  OA::OA_ptr<OA::DataFlow::ParamBindings> parambind 
      = parambindman->performAnalysis(cgraph);

  parambind->dump(std::cout,irInterface);

  // Alias 
  OA::OA_ptr<OA::Alias::ManagerInsNoPtrInterAliasMap> interaliasmapman;
  interaliasmapman = new OA::Alias::ManagerInsNoPtrInterAliasMap(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = interaliasmapman->performAnalysis(cgraph,parambind);

  interAlias->dump(std::cout,irInterface);

  // Intra Side-Effect
  OA::OA_ptr<OA::SideEffect::ManagerStandard> sideeffectman;
  sideeffectman = new OA::SideEffect::ManagerStandard(irInterface);
  // InterSideEffect
  OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
  interSEman = new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);
  OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
  interSE = interSEman->performAnalysis(cgraph, interAlias, sideeffectman);

  //interSE->dump(std::cout,irInterface);

  // CFG 
  OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
  OA::OA_ptr<OA::CFG::ManagerStandard> cfgman;
  cfgman = new OA::CFG::ManagerStandard(irInterface);
  eachCFG = new OA::CFG::EachCFGStandard(cfgman);

 
  //--------------------- InterActivity analysis
  OA::OA_ptr<OA::Activity::ManagerInterActive> activeman;
  activeman = new OA::Activity::ManagerInterActive(irInterface);
  OA::OA_ptr<OA::Activity::InterActive> active;
  active = activeman->performAnalysis(cgraph, parambind,
                interAlias, interSE, eachCFG);

  active->dump(std::cout, irInterface);
  return 0;
}
*/


static int
TestIR_OAEachActivity(std::ostream& os, PU_Info* pu_forest,
                       OA::OA_ptr<Open64IRInterface> irInterface)
{
/*
  Diag_Set_Phase("WHIRL tester: TestIR_OAEachActivity");

  // iterate over all functions and call kludge
  Open64IRProcIterator procIt(pu_forest);  

  // Alias 
  OA::OA_ptr<OA::Alias::ManagerInterAliasMapBasic> interaliasmapman;
  interaliasmapman = new OA::Alias::ManagerInterAliasMapBasic(irInterface);
  OA::OA_ptr<OA::Alias::InterAliasMap> interAlias;
  interAlias = interaliasmapman->performAnalysis();
  
  // CFG 
  OA::OA_ptr<OA::CFG::EachCFGInterface> eachCFG;
  OA::OA_ptr<OA::CFG::ManagerStandard> cfgman;
  cfgman = new OA::CFG::ManagerStandard(irInterface);
  eachCFG = new OA::CFG::EachCFGStandard(cfgman);

  // call graph
  OA::OA_ptr<OA::CallGraph::ManagerStandard> cgraphman;
  cgraphman = new OA::CallGraph::ManagerStandard(irInterface);
  OA::OA_ptr<Open64IRProcIterator> procIter;
  procIter = new Open64IRProcIterator(pu_forest);
  OA::OA_ptr<OA::CallGraph::CallGraphStandard> cgraph = 
      cgraphman->performAnalysis(procIter);

  cgraph->dump(std::cout, irInterface);

  // Intra Side-Effect
  OA::OA_ptr<OA::SideEffect::ManagerStandard> sideeffectman;
  sideeffectman = new OA::SideEffect::ManagerStandard(irInterface);

  // InterSideEffect
  OA::OA_ptr<OA::SideEffect::ManagerInterSideEffectStandard> interSEman;
  interSEman = new OA::SideEffect::ManagerInterSideEffectStandard(irInterface);

  OA::OA_ptr<OA::SideEffect::InterSideEffectStandard> interSE;
  interSE = interSEman->performAnalysis(cgraph, interAlias, sideeffectman);

  interSE->dump(std::cout, irInterface);
  
  //--------------------- EachActivity analysis
  // Get mapping of strings to handles
  OA::StrToHandle strToHandle(cgraph);
  strToHandle.dump(std::cout,irInterface);

  // get root proc
  std::string rootProcStr;
  cout << "Enter root procedure name: ";
  cin >> rootProcStr;
  OA::ProcHandle rootProc = strToHandle.getProcHandle(rootProcStr);
  irInterface->currentProc(rootProc);

  // independent location setup
  OA::OA_ptr<OA::LocSet> indepSet;
  indepSet = new OA::LocSet; 
  std::string indepStr;
  cout << "Enter independent variable name (variable in root proc): ";
  cin >> indepStr;
  OA::SymHandle sym = strToHandle.getSymHandle(indepStr);
  indepSet->insert(irInterface->getLocation(rootProc,sym));
  OA::OA_ptr<OA::LocSetIterator> indepIterPtr;
  indepIterPtr = new OA::LocSetIterator(indepSet);

  // dependent location setup
  OA::OA_ptr<OA::LocSet> depSet;
  depSet = new OA::LocSet; 
  std::string depStr;
  cout << "Enter dependent variable name: ";
  cin >> depStr;
  sym = strToHandle.getSymHandle(depStr);
  depSet->insert(irInterface->getLocation(rootProc,sym));
  OA::OA_ptr<OA::LocSetIterator> depIterPtr;
  depIterPtr = new OA::LocSetIterator(depSet);

  // now do Activity analysis on each cfg separately
  OA::OA_ptr<OA::Activity::ManagerEachActive> activeman;
  activeman = new OA::Activity::ManagerEachActive(irInterface);
  
  OA::OA_ptr<OA::Activity::InterActive> active;
  active = activeman->performAnalysis(procIter, interAlias, interSE, eachCFG);

  active->dump(std::cout, irInterface);
*/
  return 0;
}



static int
TestIR_OAUDDUChainsXAIF(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAUDDUChainsXAIF");

//    FIAlias
  OA::OA_ptr<OA::Alias::Interface> alias;
  alias  = interAlias->getAliasResults((OA::irhandle_t)pu);

     // CFG
  OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgmanstd;
  cfgmanstd = new OA::CFG::ManagerCFGStandard(irInterface);
  OA::OA_ptr<OA::CFG::CFGInterface> cfg= cfgmanstd->performAnalysis((OA::irhandle_t)pu);


  // then can do ReachDefs
  OA::OA_ptr<OA::ReachDefs::ManagerReachDefsStandard> rdman;
  rdman = new OA::ReachDefs::ManagerReachDefsStandard(irInterface);
  OA::OA_ptr<OA::ReachDefs::ReachDefsStandard> rds;
  rds = rdman->performAnalysis((OA::irhandle_t)pu,cfg,alias,interSideEffect,
                               OA::DataFlow::ITERATIVE);



  
  // then UDDUChains
  OA::OA_ptr<OA::UDDUChains::ManagerUDDUChainsStandard> udman;
  udman = new OA::UDDUChains::ManagerUDDUChainsStandard(irInterface);
  OA::OA_ptr<OA::UDDUChains::UDDUChainsStandard> udduchains= 
      udman->performAnalysis((OA::irhandle_t)pu,alias,rds,interSideEffect);
//  udduchains->dump(std::cout, irInterface);

  // and finally UDDUChainsXAIF
  OA::OA_ptr<OA::XAIF::ManagerUDDUChainsXAIF> udmanXAIF;
  udmanXAIF = new OA::XAIF::ManagerUDDUChainsXAIF(irInterface);
  OA::OA_ptr<OA::XAIF::UDDUChainsXAIF> udduchainsXAIF= 
      udmanXAIF->performAnalysis(cfg,udduchains, true);
  udduchainsXAIF->dump(std::cout, irInterface);

  return 0;
}

static int
TestIR_OAUDDUChains(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAUDDUChains");

//    FIAlias
  OA::OA_ptr<OA::Alias::Interface> alias;
  alias  = interAlias->getAliasResults((OA::irhandle_t)pu);

     // CFG
  OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgmanstd;
  cfgmanstd = new OA::CFG::ManagerCFGStandard(irInterface);
  OA::OA_ptr<OA::CFG::CFGInterface> cfg= cfgmanstd->performAnalysis((OA::irhandle_t)pu);


  // then can do ReachDefs
  OA::OA_ptr<OA::ReachDefs::ManagerReachDefsStandard> rdman;
  rdman = new OA::ReachDefs::ManagerReachDefsStandard(irInterface);
  OA::OA_ptr<OA::ReachDefs::ReachDefsStandard> rds;

  
  rds = rdman->performAnalysis((OA::irhandle_t)pu,cfg,alias,interSideEffect,
                               OA::DataFlow::ITERATIVE);

  // then UDDUChains
  OA::OA_ptr<OA::UDDUChains::ManagerUDDUChainsStandard> udman;
  udman = new OA::UDDUChains::ManagerUDDUChainsStandard(irInterface);
  OA::OA_ptr<OA::UDDUChains::UDDUChainsStandard> udduchains= 
      udman->performAnalysis((OA::irhandle_t)pu,alias,rds,interSideEffect);

  //  udduchains->dump(std::cout, irInterface);
  udduchains->output(*irInterface);

  return 0;
}

static int
TestIR_OAReachDefs(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAReachDefs");

//    FIAlias
  OA::OA_ptr<OA::Alias::Interface> alias;
  alias  = interAlias->getAliasResults((OA::irhandle_t)pu);
   
     // CFG
  OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgmanstd;
  cfgmanstd = new OA::CFG::ManagerCFGStandard(irInterface);
  OA::OA_ptr<OA::CFG::CFGInterface> cfg= cfgmanstd->performAnalysis((OA::irhandle_t)pu);
    
     
  // then can do ReachDefs
  OA::OA_ptr<OA::ReachDefs::ManagerReachDefsStandard> rdman;
  rdman = new OA::ReachDefs::ManagerReachDefsStandard(irInterface);
  OA::OA_ptr<OA::ReachDefs::ReachDefsStandard> rds;
  rds = rdman->performAnalysis((OA::irhandle_t)pu,cfg,alias,interSideEffect,
                               OA::DataFlow::ITERATIVE);
  rds->output(*irInterface);          

  return 0;
}

static int
TestIR_OAAlias_ForEachWNPU(std::ostream& os, PU_Info* pu,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
    /*
  Diag_Set_Phase("WHIRL tester: TestIR_OAAlias");

  OA::OA_ptr<OA::Alias::ManagerNoAddressOf> aliasman;
  aliasman = new OA::Alias::ManagerNoAddressOf(irInterface);
  OA::OA_ptr<OA::Alias::EquivSets> equivSets= 
      aliasman->performAnalysis((OA::irhandle_t)pu);

  equivSets->dump(std::cout, irInterface);
  */
  return 0;
}

static int
TestIR_OAAliasMapBasic_ForEachWNPU(std::ostream& os, PU_Info* pu,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAAliasMapBasic");

  // Alias analysis
  OA::OA_ptr<OA::Alias::ManagerAliasMapBasic> aliasmapman;
  aliasmapman = new OA::Alias::ManagerAliasMapBasic(irInterface);
  OA::OA_ptr<OA::Alias::AliasMap> aliasMap = 
      aliasmapman->performAnalysis((OA::irhandle_t)pu);

  //aliasMap->dump(std::cout, irInterface);
  aliasMap->output(*irInterface);
  return 0;
}

static int
TestIR_OAAliasMapXAIFBasic_ForEachWNPU(std::ostream& os, PU_Info* pu,
                         OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAAliasMapBasicXAIF");

  // Alias analysis
  OA::OA_ptr<OA::Alias::ManagerAliasMapBasic> aliasmapman;
  aliasmapman = new OA::Alias::ManagerAliasMapBasic(irInterface);
  OA::OA_ptr<OA::Alias::AliasMap> aliasMap = 
      aliasmapman->performAnalysis((OA::irhandle_t)pu);
  //aliasMap->dump(std::cout, irInterface);

  // XAIF AliasMap
  /*! commented out by PLM 08/23/06
  OA::OA_ptr<OA::XAIF::ManagerAliasMapXAIF> aliasmapxaifman;
  aliasmapxaifman = new OA::XAIF::ManagerAliasMapXAIF(irInterface);
  OA::OA_ptr<OA::XAIF::AliasMapXAIF> aliasMapXAIF = 
      aliasmapxaifman->performAnalysis((OA::irhandle_t)pu,aliasMap);

  //aliasMapXAIF->dump(std::cout, irInterface);
  aliasMapXAIF->output(*irInterface);
  */

  return 0;
}

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
    OA::OA_ptr<OA::ExprHandleIterator> exprIter;
    exprIter = ir->getExprHandleIterator(stmt);
    std::cout << "\n========================stmt============================\n";
    std::cout << "\nstmt = ";
    std::cout << ir->toString(stmt) << std::endl;

    for ( ; exprIter->isValid(); (*exprIter)++) { 
         OA::ExprHandle expr = exprIter->current();
         std::cout << "\n\t--expr----------------------------------------\n";
         std::cout << "\t  expr = " << ir->toString(expr) << std::endl;
         std::cout << "\t----------------------------------------------";
         OA::OA_ptr<OA::ExprTree> eTreePtr = ir->getExprTree(expr);
         eTreePtr->output(*ir);
    }

  }
  return 0;
}


static int
TestIR_OAReachConsts(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAReachConsts");

  // CFG
  OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgmanstd;
  cfgmanstd = new OA::CFG::ManagerCFGStandard(irInterface);
  OA::OA_ptr<OA::CFG::CFGInterface> cfg= cfgmanstd->performAnalysis((OA::irhandle_t)pu);

  // Alias analysis
  OA::OA_ptr<OA::Alias::ManagerAliasMapBasic> aliasmapman;
  aliasmapman = new OA::Alias::ManagerAliasMapBasic(irInterface);
  OA::OA_ptr<OA::Alias::Interface> alias = 
      aliasmapman->performAnalysis((OA::irhandle_t)pu);

  // then can do ReachConsts
    
  OA::OA_ptr<OA::ReachConsts::ManagerReachConstsStandard> rcman;
  rcman = new OA::ReachConsts::ManagerReachConstsStandard(irInterface);
  OA::OA_ptr<OA::ReachConsts::ReachConstsStandard> reachConsts= 
    rcman->performAnalysis((OA::irhandle_t)pu,cfg,alias,interSideEffect,
                           OA::DataFlow::ITERATIVE);

  //reachConsts->dump(std::cout, irInterface);
  reachConsts->output(*irInterface);
  
  return 0;
}

static int
TestIR_OASideEffect_ForEachWNPU(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OASideEffect_ForEachWNPU");

  // Alias analysis
  OA::OA_ptr<OA::Alias::ManagerAliasMapBasic> aliasmapman;
  aliasmapman = new OA::Alias::ManagerAliasMapBasic(irInterface);
  OA::OA_ptr<OA::Alias::Interface> alias = 
      aliasmapman->performAnalysis((OA::irhandle_t)pu);

  // Interprocedural Side-Effect Analysis
  // for now generate default conservative interprocedural side-effect results
  OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect;
  interSideEffect = new OA::SideEffect::InterSideEffectStandard;

  // Intraprocedural Side-Effect Analysis
  OA::OA_ptr<OA::SideEffect::ManagerSideEffectStandard> sideman;
  sideman = new OA::SideEffect::ManagerSideEffectStandard(irInterface);
  OA::OA_ptr<OA::SideEffect::SideEffectStandard> sideEffect = 
      sideman->performAnalysis((OA::irhandle_t)pu,alias,interSideEffect);

  sideEffect->dump(std::cout, irInterface);
  return 0;
}

static int
TestIR_OAActivity_ForEachWNPU(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::SideEffect::InterSideEffectInterface> interSideEffect)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OAActivity");

  /*
  std::map<std::string,OA::SymHandle> varToSymMap;

  // create mapping of variable names to symbol table entries
  OA::OA_ptr<OA::IRSymIterator> symIterPtr;
  symIterPtr = irInterface->getVisibleSymIterator((OA::irhandle_t)pu);
  for ( ; symIterPtr->isValid(); (*symIterPtr)++ ) {
    OA::SymHandle h = symIterPtr->current();
    ST* st = (ST*)h.hval();
    if (st) {
      varToSymMap[ST_name(st)] = h;

      std::cout << "SymHandle (hval=" << h.hval() << " = " << irInterface->toString(h);
      std::cout << ", ST_name(st) =" << ST_name(st) << "=" << std::endl;
    }

  }

  // Alias analysis
  OA::OA_ptr<OA::Alias::ManagerAliasMapBasic> aliasmapman;
  aliasmapman = new OA::Alias::ManagerAliasMapBasic(irInterface);
  OA::OA_ptr<OA::Alias::AliasMap> alias = 
      aliasmapman->performAnalysis((OA::irhandle_t)pu);
  alias->dump(std::cout, irInterface);

  // CFG
  OA::OA_ptr<OA::CFG::ManagerStandard> cfgmanstd;
  cfgmanstd = new OA::CFG::ManagerStandard(irInterface);
  OA::OA_ptr<OA::CFG::Interface> cfg= cfgmanstd->performAnalysis((OA::irhandle_t)pu);

  // Get mapping of strings to handles
  OA::StrToHandle strToHandle((OA::irhandle_t)pu);

  // independent location setup
  OA::OA_ptr<OA::LocSet> indepSet;
  indepSet = new OA::LocSet; 
  std::string indepStr;
  OA::ProcHandle proc = (OA::irhandle_t)pu;
  cout << "Enter independent variable name for procedure " 
       << irInterface->toString(proc) << ": ";
  cin >> indepStr;
  OA::SymHandle sym = strToHandle.getSymHandle(indepStr);
  OA::OA_ptr<OA::Location> loc = 
      irInterface->getLocation((OA::irhandle_t)pu,sym);
  if (!loc.ptrEqual(NULL)) {
    indepSet->insert(irInterface->getLocation((OA::irhandle_t)pu,sym));
  } 
  OA::OA_ptr<OA::LocSetIterator> indepIterPtr;
  indepIterPtr = new OA::LocSetIterator(indepSet);

  // dependent location setup
  OA::OA_ptr<OA::LocSet> depSet;
  depSet = new OA::LocSet; 
  std::string depStr;
  cout << "Enter dependent variable name: ";
  cin >> depStr;
  sym = strToHandle.getSymHandle(depStr);
  depSet->insert(irInterface->getLocation((OA::irhandle_t)pu,sym));
  OA::OA_ptr<OA::LocSetIterator> depIterPtr;
  depIterPtr = new OA::LocSetIterator(depSet);

  // Active
  OA::OA_ptr<OA::Activity::ManagerActiveStandard> activeman;
  activeman = new OA::Activity::ManagerActiveStandard(irInterface);

  // if no independent var set then call performAnalysis that makes conservative
  // assumption
  assert(0);  // broken since added InterActivity
//  OA::OA_ptr<OA::Activity::ActiveStandard> active;
//  if (indepSet->empty()) {
//    active = activeman->performAnalysis((OA::irhandle_t)pu, cfg, alias, 
//                                     interSideEffect); 
//  } else {
//    active = activeman->performAnalysis((OA::irhandle_t)pu, cfg, alias, 
//                                     interSideEffect, indepIterPtr, depIterPtr);
//  }
                
//  active->dump(std::cout, irInterface);
*/
 
  return 0;
}

static int
TestIR_OACommonBlockVars(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface)
{
  Diag_Set_Phase("WHIRL tester: TestIR_OACommonBlockVars");

  /* no longer have getVisibleSymIterator
   
  //typedef struct {
  //    char * sym_name;
  //    char * common_name;
  //} fully_qualified_name;
  typedef std::pair<char*,char*> fully_qualified_name;

  // iterate over all the procedures
  Open64IRProcIterator procIt(pu);
  for ( ; procIt.isValid(); ++procIt) { 
    
    // The PU_Info* for this PU
    //PU_Info* pu = (PU_Info*)procIt.current().hval();
    OA::ProcHandle proc = procIt.current();

    // iterate over all the visible symbols
    OA::OA_ptr<OA::IRSymIterator> symIter 
        = irInterface->getVisibleSymIterator(proc);
    for ( ; symIter->isValid(); (*symIter)++ ) {
        OA::SymHandle sym = symIter->current();
        ST* st = (ST*)sym.hval();
        
        std::cout << "Symbol: " << irInterface->toString(sym) << std::endl;
        std::cout << "\tStab_Is_Based_At_Common_Or_Equivalence(sym) = "
                  << Stab_Is_Based_At_Common_Or_Equivalence((ST*)sym.hval())
                  << std::endl;
        std::cout << "\tStab_Is_Common_Block(sym) = "
                  << Stab_Is_Common_Block((ST*)sym.hval())
                  << std::endl;
                  

        if (Stab_Is_Based_At_Common_Or_Equivalence(st) ) {
            ST* tempst = st;

            while (!Stab_Is_Common_Block(tempst)) {
                tempst = ST_base(tempst);
            }
            ST* base_st = tempst;

            //// make a pair for fully-qualified name
            fully_qualified_name fqn 
                = (fully_qualified_name)std::make_pair(ST_name(st),
                                                       ST_name(base_st));
            std::cout << "fully_qualified_name = " << fqn.first << ", "
                      << fqn.second << std::endl;

            OA::OA_ptr<OA::Location> loc = irInterface->getLocation(proc,sym);
            loc->dump(std::cout,irInterface);
        }

    }
  }

  return 0;
  */
}

static int
TestIR_OALinearity(std::ostream& os, PU_Info* pu,
            OA::OA_ptr<Open64IRInterface> irInterface,
            OA::OA_ptr<OA::Alias::InterAliasMap> interAlias,
            OA::OA_ptr<OA::DataFlow::ParamBindings> parambind)
{
    std::cout << "Linearity Analysis Start:\n";
  
    OA::OA_ptr<Open64IRProcIterator> procIter;
    procIter = new Open64IRProcIterator(pu);

    //FIAlias
    OA::OA_ptr<OA::Alias::Interface> alias;
    alias  = interAlias->getAliasResults((OA::irhandle_t)pu);

    // CFG
    OA::OA_ptr<OA::CFG::ManagerCFGStandard> cfgmanstd;
    cfgmanstd = new OA::CFG::ManagerCFGStandard(irInterface);
    OA::OA_ptr<OA::CFG::CFGInterface> cfg= cfgmanstd->performAnalysis((OA::irhandle_t)pu);

    OA::OA_ptr<OA::Linearity::ManagerLinearity> linmanstd;
    linmanstd = new OA::Linearity::ManagerLinearity(irInterface);
  
/*  OA::OA_ptr<OA::Linearity::LinearityMatrix> LM
     = linmanstd->performAnalysis((OA::irhandle_t)pu);
  LM->output(*irInterface);

  OA::OA_ptr<OA::Linearity::LinearityMatrix> LM2
     = linmanstd->performAnalysis2((OA::irhandle_t)pu);
  LM2->output(*irInterface);
*/
    OA::OA_ptr<OA::Linearity::LinearityMatrix> LM3
       = linmanstd->performAnalysis((OA::irhandle_t)pu,cfg,alias,parambind,
                                    OA::DataFlow::ITERATIVE);

    LM3->output(*irInterface);

  
    return 0;
}



//***************************************************************************
// just seeing if various calls to graph methods compile, tricky because
// of multiple inheritance
#include <OpenAnalysis/Utils/DGraph/DGraphImplement.hpp>

void graphCompile()
{
    /*! Commented out by PLM 08/23/06
    OA::OA_ptr<OA::CFG::Node> cn1, cn2;
    cn1 = new OA::CFG::Node;
    cn2 = new OA::CFG::Node;
    if (cn1==cn2) {}

    OA::OA_ptr<OA::DGraph::DGraphStandard::Node> n1, n2;
    n1 = new OA::DGraph::DGraphStandard::Node;
    n2 = new OA::DGraph::DGraphStandard::Node;

    int id = n1->getId();
    int num = n1->num_incoming();
    num = n1->num_outgoing();

    if (n1==n2) { }
    if (n1<n2) { }
    if (n1==cn1) {}
    if (cn1==n1) {}
    if (n1<cn1) {}
    if (cn1<n1) {}

    OA::OA_ptr<OA::BaseGraph::Node> b1, b2;
    b1 = new OA::BaseGraph::Node;
    b2 = new OA::BaseGraph::Node;

    if (cn1==b1) { }
    if (b1==cn1) { }
    if (b1==b2) { }
    if (cn1<b1) { }
    if (b1<cn1) { }
    if (b1<b2) { }

    if (n1==b1) { }
    if (b1==n1) { }
    if (b1==b2) { }
    if (n1<b1) { }
    if (b1<n1) { }
    if (b1<b2) { }

//--------------------------------------------
    OA::OA_ptr<OA::CFG::CFGStandard::Edge> ce1, ce2;
    ce1 = new OA::CFG::CFGStandard::Edge(cn1,cn2,
            OA::CFG::Interface::FALLTHROUGH_EDGE,0);
    ce2 = new OA::CFG::CFGStandard::Edge(cn1,cn2,
            OA::CFG::Interface::FALLTHROUGH_EDGE,0);
    if (ce1==ce2) {}
    if (ce1<ce2) {}

    OA::OA_ptr<OA::DGraph::DGraphStandard::Edge> e1, e2;
    e1 = new OA::DGraph::DGraphStandard::Edge(n1,n2);
    e2 = new OA::DGraph::DGraphStandard::Edge(n1,n2);

    id = e1->getId();

    if (e1==e2) { }
    if (e1<e2) { }
    if (e1==ce1) {}
    if (ce1==e1) {}
    if (e1<ce1) {}
    if (ce1<e1) {}

    OA::OA_ptr<OA::BaseGraph::Edge> be1, be2;
    be1 = new OA::BaseGraph::Edge(n1,n2);
    be2 = new OA::BaseGraph::Edge(n1,n2);

    if (ce1==be1) { }
    if (be1==ce1) { }
    if (be1==be2) { }
    if (ce1<be1) { }
    if (be1<ce1) { }
    if (be1<be2) { }

    if (e1==be1) { }
    if (be1==e1) { }
    if (be1==be2) { }
    if (e1<be1) { }
    if (be1<e1) { }
    if (be1<be2) { }
*/
}
