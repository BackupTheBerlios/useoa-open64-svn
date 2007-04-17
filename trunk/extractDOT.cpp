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
// Author:  B. Kreaseck
// Date:    4/17/07
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
    exit(0);
  }
  
  ifstream inFile(argv[1]);
  if (inFile.fail()) {
    cout << "Unable to open '" << argv[1] << "' for input. Exitting ...\n\n";
    exit(0);
  }
  
  bool graphStarted = false;
  string line;
  getline(inFile,line);
  while (!inFile.eof()) {
    if (line.compare(0,7,"digraph") == 0) {
      graphStarted = true;
    }
    if (graphStarted) {
      // look for Pragma
      int pos = line.find("PRAGMA",0);
      if (pos != string::npos) {  // then we found a PRAGMA
        // strip off from the PRAGMA position to the end
        line.erase(pos);
        string line2;
        // get next line if possible
        if (!inFile.eof()) {
          getline(inFile,line2);
        }
        // concatenate
        line = line+line2;
      }
      // send line to stdout
      cout << line.c_str() << endl;
    }
    // get the next line if possible
    getline(inFile,line);
  }
  
  inFile.close();
}
