/*! \file
  
  \brief Implementation of StrToHandle

  \author Michelle Strout
  \version $Id: StrToHandle.cpp,v 1.3 2005/06/11 02:37:43 mstrout Exp $

  Copyright (c) 2002-2004, Rice University <br>
  Copyright (c) 2004, University of Chicago <br>  
  All rights reserved. <br>
  See ../../../Copyright.txt for details. <br>

*/

#include "StrToHandle.hpp"

namespace OA {

StrToHandle::StrToHandle(ProcHandle proc)
{
    initMapping(proc);
}

void StrToHandle::initMapping(ProcHandle proc)
{
    OA_ptr<Open64IRInterface> ir; ir = new Open64IRInterface;

    // create mapping of variable names for that procedure
    // to symbol table entries  
    OA::OA_ptr<OA::IRSymIterator> symIterPtr;
    symIterPtr = ir->getVisibleSymIterator(proc);
    for ( ; symIterPtr->isValid(); (*symIterPtr)++ ) {
      OA::SymHandle h = symIterPtr->current();
      ST* st = (ST*)h.hval();
      if (st) {
        mStrToSym[ST_name(st)] = h;
      }
    }  
}
 
StrToHandle::StrToHandle(OA_ptr<CallGraph::CallGraphStandard> callGraph)
{
  OA_ptr<Open64IRInterface> ir; ir = new Open64IRInterface;

  // loop through all the nodes in the CallGraph
  OA_ptr<CallGraph::CallGraphStandard::NodesIterator> nodeIterPtr
      = callGraph->getNodesIterator();
  for ( ; nodeIterPtr->isValid(); ++(*nodeIterPtr) ) {
    OA_ptr<CallGraph::CallGraphStandard::Node> node = nodeIterPtr->current();
  
    // if the procedure node is defined then has a proc handle
    if (node->isDefined()) {
      ProcHandle proc = node->getProc();
      //ir->currentProc(proc);
      SymHandle procSym = node->getProcSym();
      ST* st = (ST*)procSym.hval();
      if (st) {
        mStrToProc[ST_name(st)] = proc;
        mStrToSym[ST_name(st)] = procSym;
      }

      // create mapping of variable names for that procedure
      // to symbol table entries  
      initMapping(proc);
    }
  }

}


//*****************************************************************
// Output
//*****************************************************************
void StrToHandle::dump(std::ostream& os, OA_ptr<IRHandlesIRInterface> ir)
{
    os << "====================== StrToHandle" << std::endl;

    // Loop through both maps and generate output
    os << "Mapping of strings to procedure handles" << std::endl;
    std::map<std::string,ProcHandle>::iterator mapIter;
    for (mapIter=mStrToProc.begin(); mapIter!=mStrToProc.end(); mapIter++) {
      os << "\tstr = " << mapIter->first << ", procHandle = " 
         << ir->toString(mapIter->second) << std::endl;
    }

    os << "Mapping of strings to symbol handles" << std::endl;
    std::map<std::string,SymHandle>::iterator mapIterSym;
    for (mapIterSym=mStrToSym.begin(); mapIterSym!=mStrToSym.end(); 
         mapIterSym++) 
    {
      os << "\tstr = " << mapIterSym->first << ", symHandle = " 
         << ir->toString(mapIterSym->second) << std::endl;
    }

    os << std::endl;
}



} // end of OA namespace
