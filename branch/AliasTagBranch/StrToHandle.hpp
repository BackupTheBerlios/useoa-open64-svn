/*! \file
  
  \brief Maps strings to IRHandles, specifically ProcHandles and
         SymHandles.

  \authors Michelle Strout
  \version $Id: StrToHandle.hpp,v 1.2 2005/02/04 22:25:25 mstrout Exp $

  Copyright (c) 2002-2004, Rice University <br>
  Copyright (c) 2004, University of Chicago <br>  
  All rights reserved. <br>
  See ../../../Copyright.txt for details. <br>

*/

#ifndef StrToHandle_hpp
#define StrToHandle_hpp

#include <cassert>
#include <iostream>
#include <string>
#include <map>
#include <algorithm>
#include <cctype>
#include <OpenAnalysis/IRInterface/IRHandles.hpp>
#include <OpenAnalysis/CallGraph/CallGraphStandard.hpp>
#include <OpenAnalysis/IRInterface/AccessLocIRInterface.hpp>

// For now this class is really specialized for Open64
#include "Open64IRInterface/Open64IRInterface.hpp"

namespace OA {


class StrToHandle {
  public:
    StrToHandle (ProcHandle proc);
    StrToHandle (OA_ptr<CallGraph::CallGraphStandard> callGraph);
    ~StrToHandle () {}

    void initMapping(ProcHandle proc);

    //! Return the ProcHandle for the given procedure string
    ProcHandle getProcHandle(std::string str)
      { if (mStrToProc[str] == SymHandle(0)) {
          return mStrToProc[str+"_"]; 
        }
        return mStrToProc[str]; 
      }

    //! Return the SymHandle for a given identifier
    SymHandle getSymHandle(std::string str)
      { if (mStrToSym[str] == SymHandle(0)) {
          std::transform(str.begin(),str.end(), str.begin(),
                         (int(*)(int))toupper);
        }
        return mStrToSym[str]; 
      }

    //*****************************************************************
    // Output
    //*****************************************************************

    void dump(std::ostream& os, OA_ptr<IRHandlesIRInterface> ir);

  private:
    // mapping of strings to Procedures
    std::map<std::string,ProcHandle> mStrToProc;
    // mapping of strings to Procedures
    std::map<std::string,SymHandle> mStrToSym;

};

} // end of OA namespace

#endif

