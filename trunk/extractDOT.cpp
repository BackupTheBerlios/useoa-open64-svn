// This program extracts the DOT lines from an output file.
// In the process, it removes the PRAGMA line portions that Open64 spits into
// the normal output during the printing of the first statement in a procedure.
// The PRAGMA line portion comes with a carriage-return that makes a cut and
// paste into GraphViz impossible.
// The extracted DOT lines are printed to stdout
// Usage: 
//   extractDOT outputFileName > outputFileName.dot
//   or
//   extractDOT outputFileName
//
//   the outputFileName is required.
// Author:   B. Kreaseck
// Date:     4/17/07
//-----------
// Modified: 6/7/07
//  LABELs and LOCs lines interrupt in the same way that PRAGMA does.
//  Also, sometimes the digraph is not at the end of the file.
//-----------
//--------------------------------------------------------------------------

#include <string>
#include <iostream>
#include <fstream>

using namespace std;
using std::string;

main(int argc, char* argv[])
{
  
  if (argc != 2) {
    cout << "Usage:\n\textractDot outputFileName\n\tor\n\t"
         << "extractDot outputFileName > outputFileName.dot\n\n"
         << "outputFileName is required.\n\n";
    return(0);
  }
  
  ifstream inFile(argv[1]);
  if (inFile.fail()) {
    cout << "Unable to open '" << argv[1] << "' for input. Exiting ...\n\n";
    return(0);
  }
  
  bool graphStarted = false;
  string line;
  getline(inFile,line);
  while (!inFile.eof() && !graphStarted) {
    if (line.compare(0,7,"digraph") == 0) {
      graphStarted = true;
    } else {
      getline(inFile,line);
    }
  }

  bool graphFinished = false;
  while (!inFile.eof() && !graphFinished) {
    // look for Pragma, LOC, LABEL, etc.
    int posP  = line.find("PRAGMA",0);
    int posL  = line.find(" LOC 0 0 source files:",0);
    int posLL = line.find("LABEL L",0);
    int posI  = line.find("I8SRCTRIPLET",0);
    while(posL  != string::npos || 
          posP  != string::npos ||
          posLL != string::npos ||
          posI  != string::npos) {
      if (posL != string::npos) {  // then we found a LOC
        // strip off from the LOC position to the end
        line.erase(posL);
        string line2;
        // get next line if possible
        if (!inFile.eof()) {
          getline(inFile,line2);
        }
        // concatenate
        line = line+line2;
      } 
      else if (posP != string::npos) {  // then we found a PRAGMA
        // strip off from the PRAGMA position to the end
        line.erase(posP);
        string line2;
        // get next line if possible
        if (!inFile.eof()) {
          getline(inFile,line2);
        }
        // concatenate
        line = line+line2;
      } 
      else if (posLL != string::npos) {  // then we found a LABEL
        // strip off from the LABEL position to the end
        line.erase(posLL);
        string line2;
        // get next line if possible
        if (!inFile.eof()) {
          getline(inFile,line2);
        }
        // concatenate
        line = line+line2;
      }
      else if (posI != string::npos) {  // then we found a I8SRCTRIPLET
        // strip off from the I8SRCTRIPLET position to the end
        line.erase(posI);
        string line2;
        // get next line if possible
        if (!inFile.eof()) {
          getline(inFile,line2);
        }
        // concatenate
        line = line+line2;
      } 
      posP  = line.find("PRAGMA",0);
      posL  = line.find(" LOC 0 0 source files:",0);
      posLL = line.find("LABEL L",0);
      posI  = line.find("I8SRCTRIPLET",0);
    }
    // send line to stdout
    cout << line.c_str() << endl;

    // get the next line if possible
    getline(inFile,line);

    if (!inFile.eof()) {
      if (line.size() > 0) {
        if (line.compare(0,1," ") != 0) {
          graphFinished = true;  // time to stop
        }
      }
    }
  }
  
  inFile.close();
}
