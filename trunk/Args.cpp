// -*-Mode: C++;-*-
// $Header: /home/derivs2/mstrout/CVSRepository/UseNewOA-Open64/Args.cpp,v 1.17 2005/06/11 02:37:43 mstrout Exp $
// * BeginRiceCopyright *****************************************************
// ******************************************************* EndRiceCopyright *

//***************************************************************************
//
// File:
//   $Source: /home/derivs2/mstrout/CVSRepository/UseNewOA-Open64/Args.cpp,v $
//
// Purpose:
//    [The purpose of this file]
//
// Description:
//    [The set of functions, macros, etc. defined in the file]
//
//***************************************************************************

//************************* System Include Files ****************************

//*************************** User Include Files ****************************

#include "Args.hpp"

//*************************** Forward Declarations **************************

using std::cerr;
using std::endl;
using std::string;


//***************************************************************************

static const char* version_info = "version .1";

static const char* usage_summary =
"[mode] [options] <whirl-file>\n";

static const char* usage_details =
"Given a WHIRL file and a mode, do something.\n"
"\n"
"Modes:\n"
"      --oa-CFG        test OA CFG\n"
"      --oa-MemRefExpr test OA MemRefExpr analysis\n"
//"      --oa-Alias      test OA Alias analysis\n"
"      --oa-CallGraph  test OA CallGraph analysis\n"
"      --oa-ReachDefs  test OA ReachDefs analysis\n"
"      --oa-UDDUChains test OA UDDUChains analysis\n"
"      --oa-UDDUChainsXAIF 	test OA UDDUChainsXAIF analysis\n"
"      --oa-ExprTree   test source IR ExprTree creation\n"
"      --oa-ReachConsts  	test OA ReachConsts\n"
//"      --oa-AliasMapXAIF 	test OA AliasMapXAIF analysis\n"
"      --oa-SideEffect  	test OA SideEffect\n"
"      --oa-InterSideEffect  	test OA InterSideEffect\n"
"      --oa-Activity        	test Activity Analysis \n"
"      --oa-EachActivity  	test Activity analysis where specify indep/dep for root proc and others use side-effect estimate (currently broken)\n"
"      --oa-InterActivity  	test interprocedural Activity analysis\n"
"      --oa-ICFGActivity  	test interprocedural Activity analysis\n"
"      --oa-ParamBindings  test analysis of parameter bindings\n"
"      --oa-InvisibleSymMapBottom  test analysis that generates pessimistic InvisibleSymMap\n"
"      --oa-InterDep    test differentiable deps analysis\n"
"      --oa-ICFG        test ICFG analysis\n"
"      --oa-CommonBlock        learning about common blocks\n"
"      --oa-AliasMapBasic   test OA AliasMapBasic analysis\n"
"      --oa-AliasMapInter   test OA ManagerInsNoPtrInterAliasMap analysis\n"
"      --oa-AliasMapXAIFBasic   test OA AliasMapXAIF from AliasMapBasic analysis\n"
"      --oa-AliasMapXAIFInter   test OA AliasMapXAIF from ManagerInsNoPtrInterAliasMap analysis\n"
"\n"
"Options:\n"
"  -d, --dump          dump the WHIRL IR\n"
"  -V, --version       print version information\n"
"  -h, --help          print this help\n"
"      --debug [lvl]   debug mode at level `lvl'\n";
//"      --ir            run a test routine on IR\n"
//"      --oa-ujnum      test OA UJ numbering\n"
//"      --whirl2f       test whirl2f\n"


#define CLP CmdLineParser

CmdLineParser::OptArgDesc Args::optArgs[] = {
  // Modes
  {  0 , "oa-CFG",            CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-MemRefExpr",     CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  //{  0 , "oa-Alias",          CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  //{  0 , "oa-AliasMap",       CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-CallGraph",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-ReachDefs",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-UDDUChains",     CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-UDDUChainsXAIF", CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-ExprTree",         CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-ReachConsts",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  //{  0 , "oa-AliasMapXAIF",       CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-SideEffect",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-InterSideEffect",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-Activity",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-EachActivity",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-InterActivity",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-ICFGActivity",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-ParamBindings",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-InvisibleSymMapBottom",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-InterDep",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-ICFG",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-CommonBlock",      CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-AliasMapBasic",       CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-AliasMapInter",       CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-AliasMapXAIFBasic",       CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  {  0 , "oa-AliasMapXAIFInter",       CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
//  {  0 , "ir",         CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
//  {  0 , "oa-ujnum",   CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
//  {  0 , "whirl2f",    CLP::ARG_NONE, CLP::DUPOPT_ERR,  NULL },
  
  // Options
  { 'd', "dump",       CLP::ARG_NONE, CLP::DUPOPT_CLOB, NULL },
  { 'V', "version",    CLP::ARG_NONE, CLP::DUPOPT_CLOB, NULL },
  { 'h', "help",       CLP::ARG_NONE, CLP::DUPOPT_CLOB, NULL },
  {  0 , "debug",      CLP::ARG_OPT,  CLP::DUPOPT_CLOB, NULL },
  CmdLineParser::OptArgDesc_NULL
};

#undef CLP

//***************************************************************************
// Args
//***************************************************************************

Args::Args()
{
  Ctor();
}

Args::Args(int argc, const char* const argv[])
{
  Ctor();
  Parse(argc, argv);
}

void
Args::Ctor()
{
  debug = 0;      // default: 0 (off)
  dumpIR = false;
}

Args::~Args()
{
}


void 
Args::PrintVersion(std::ostream& os) const
{
  os << GetCmd() << ": " << version_info << endl;
}


void 
Args::PrintUsage(std::ostream& os) const
{
  os << "Usage: " << GetCmd() << " " << usage_summary << endl
     << usage_details << endl;
} 


void 
Args::PrintError(std::ostream& os, const char* msg) const
{
  os << GetCmd() << ": " << msg << endl
     << "Try `" << GetCmd() << " --help' for more information." << endl;
}

void 
Args::PrintError(std::ostream& os, const std::string& msg) const
{
  PrintError(os, msg.c_str());
}


void
Args::Parse(int argc, const char* const argv[])
{
  try {
    // -------------------------------------------------------
    // Parse the command line
    // -------------------------------------------------------
    parser.Parse(optArgs, argc, argv);

    // -------------------------------------------------------
    // Sift through results, checking for semantic errors
    // -------------------------------------------------------

    // Special options that should be checked first
    if (parser.IsOpt("debug")) { 
      debug = 1; 
      if (parser.IsOptArg("debug")) {
	const string& arg = parser.GetOptArg("debug");
	debug = (int)CmdLineParser::ToLong(arg);
      }
    }
    if (parser.IsOpt("help")) { 
      PrintUsage(std::cerr); 
      exit(1);
    }
    if (parser.IsOpt("version")) { 
      PrintVersion(std::cerr);
      exit(1);
    }
    
    // Check for mode
//    if (parser.IsOpt("ir")) { runMode = 1; }
//    if (parser.IsOpt("oa-ujnum")) { runMode = 2; }
//    if (parser.IsOpt("whirl2f")) { runMode = 3; }
    if (parser.IsOpt("oa-CFG")) { runMode = 1; }
    if (parser.IsOpt("oa-MemRefExpr")) { runMode = 2; }
    //if (parser.IsOpt("oa-Alias")) { runMode = 3; }
    //if (parser.IsOpt("oa-AliasMap")) { runMode = 4; }
    if (parser.IsOpt("oa-CallGraph")) { runMode = 5; }
    if (parser.IsOpt("oa-ReachDefs")) { runMode = 6; }
    if (parser.IsOpt("oa-UDDUChains")) { runMode = 7; }
    if (parser.IsOpt("oa-UDDUChainsXAIF")) { runMode = 8; }
    if (parser.IsOpt("oa-ExprTree")) { runMode = 9; }
    if (parser.IsOpt("oa-ReachConsts")) { runMode = 10; }
    //if (parser.IsOpt("oa-AliasMapXAIF")) { runMode = 11; }
    if (parser.IsOpt("oa-SideEffect")) { runMode = 12; }
    if (parser.IsOpt("oa-InterSideEffect")) { runMode = 13; }
    if (parser.IsOpt("oa-Activity")) { runMode = 14; }
    if (parser.IsOpt("oa-EachActivity")) { runMode = 15; }
    if (parser.IsOpt("oa-ParamBindings")) { runMode = 16; }
    if (parser.IsOpt("oa-InvisibleSymMapBottom")) { runMode = 17; }
    if (parser.IsOpt("oa-InterDep")) { runMode = 18; }
    if (parser.IsOpt("oa-InterActivity")) { runMode = 19; }
    if (parser.IsOpt("oa-ICFG")) { runMode = 20; }
    if (parser.IsOpt("oa-ICFGActivity")) { runMode = 21; }
    if (parser.IsOpt("oa-CommonBlock")) { runMode = 22; }
    if (parser.IsOpt("oa-AliasMapBasic")) { runMode = 23; }
    if (parser.IsOpt("oa-AliasMapInter")) { runMode = 24; }
    if (parser.IsOpt("oa-AliasMapXAIFBasic")) { runMode = 25; }
    if (parser.IsOpt("oa-AliasMapXAIFInter")) { runMode = 26; }
    
    // Check for other options
    if (parser.IsOpt("dump")) { dumpIR = true; }
    
    // Check for required arguments
    if (parser.GetNumArgs() != 1) {
      PrintError(std::cerr, "Invalid number of arguments!");
      exit(1);
    }
    whirlFileNm = parser.GetArg(0);
  } 
  catch (CmdLineParser::ParseError& e) {
    PrintError(std::cerr, e.GetMessage());
    exit(1);
  }
}


void 
Args::Dump(std::ostream& os) const
{
  parser.Dump(os);
}

void 
Args::DDump() const
{
  Dump(std::cerr);
}

