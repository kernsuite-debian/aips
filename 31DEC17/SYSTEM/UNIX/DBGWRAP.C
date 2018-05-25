/*--------------------------------------------------------------------*/
/*! Wrapper program for debuggers called from AIPS                    */
/*# Util                                                              */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1997                                               */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
/*  dbgwrap is intended to be specified as the debugger for AIPS when */
/*  it is run in debug mode.  dbgwrap prompts the user for the name   */
/*  of a debugger to use and for its command line arguments and then  */
/*  executes the program image named as its first and only command-   */
/*  line argument under the given debugger command.  If the user      */
/*  supplies an empty command string then dbgwrap terminates without  */
/*  running a debugger and returns control to AIPS.                   */
/*                                                                    */
/*  This allows the user to switch between any number of debugging or */
/*  monitoring tools without restarting AIPS and to supply arguments  */
/*  to debugging programs.                                            */
/*--------------------------------------------------------------------*/

#include <unistd.h>
#include <algo.h>
#include <assert.h>
#include <errno.h>
#include <iostream.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <vector.h>

typedef char* string;

// Prompt the user for the name of a debugger and any command line
// arguments (excluding the name of the program to be run under the
// debugger) and return the supplied command as a list of words.
vector<string>* read_command()
{
    vector<string>* result = new vector<string>;

    char buffer[_POSIX_MAX_INPUT];	// buffer for input strings
    string argument = NULL;		// a single argument

    // Prompt the user for the debugger command:

    cout << "Enter the debugger command (including any command-line flags)."
         << endl;
    cout << "Command: " << flush;

    // Parse the reply:

    while (cin.peek() != EOF && cin.peek() != '\n')
    {
        // Skip whitespace until reaching a non-blank character or the
        // end of the line:

        while (cin.peek() != EOF && cin.peek() != '\n' && (cin.peek() == ' ' || cin.peek() == '\t'))
        {
            cin.get();
        }

        // (cin.peek() != EOF && cin.peek() != '\n') implies the next
        // character is not blank and there is at least one more word
        // to read.

        if (cin.peek() != EOF && cin.peek() != '\n')
        {
            // Extract the next word:

            cin >> buffer;

            assert(strlen(buffer) > 0);

            // Allocate new storage to it so that it will not be
            // overwritten:

            argument = new char[strlen(buffer) + 1];
            strcpy(argument, buffer);

            result->push_back(argument);
        }
    }

    return result;
}

// Prompt for a debugger command (including any command line flags);
// if the command is not blank then execute argv[1] under that debugger
// otherwise terminate immediately and return EXIT_SUCCESS.  Return
// exit failure if there is not exectly one argument to the command.

int main(int argc, string argv[])
{
    int result;                        // status value to be returned to the
                                       // parent process
    vector<string>* arguments = NULL;  // debugger command and arguments
    string* argument_array = NULL;     // packed argument array for execvp();

    if (argc == 2)              // process name is included in argc
    {
        assert(argv[1] > 0);

        arguments = read_command();

        if (arguments->size() > 0)
        {
            arguments->push_back(argv[1]);       // program to run
            arguments->push_back((string) NULL); // end marker

            // The size of the argument list is now known. Pack
            // the arguments into an array:

            argument_array = new string[arguments->size()];
            copy(arguments->begin(), arguments->end(), argument_array);

            // Execute the debugger:

            errno = 0;
            if (execvp(argument_array[0], argument_array) == -1)
            {
                cerr << "Could not execute " << argument_array[0]
                     << ": " << strerror(errno) << endl;
                result = EXIT_FAILURE;
            }
        }
        else
        {
            cout << "No command specified - returning to AIPS." << endl;
            result = EXIT_SUCCESS;
        }
    }
    else
    {
        cerr << "Faulty argument count: " << argc << endl;
        result = EXIT_FAILURE;
    }

    return result;
}
