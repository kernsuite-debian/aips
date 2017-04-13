#define Z_grlhi__
#include <stdio.h>
#include <stdlib.h>
#include <NOTST/GNU/history.h>

#define HAVE_STRING_H
#include <NOTST/GNU/readline.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zgrlhi_(int *opcode, char phname[MAXPNAME], int *ierr)
#else
   zgrlhi_(opcode, phname, ierr)
   int *opcode;
   char phname[MAXPNAME];
   int *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! set up use of GNU readline library for POPS command line          */
/*# Terminal                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1998                                               */
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
/* ZGRLHI initializes the GNU readline library for AIPS, unless       */
/* disabled by starting AIPS with the "norl" option.                  */
/*                                                                    */
/* Input:                                                             */
/*    OPCODE I      1 => open, 2 => close                             */
/*    PHNAME C*48   Physical filename                                 */
/* Output:                                                            */
/*    IERR   I      Error return code: 0 => ok                        */
/*                     <other> => error or disabled                   */
/*                                                                    */
/* Generic UNIX version for OS's implementing GNU's libreadline       */
/*                                                                    */
/* JAU 26 Jul 95: Jeff Uphoff, NRAO.                                  */
/* JAU 29 Aug 95: Jeff Uphoff, NRAO.  Added "norl" disable option.    */
/* JAU 22 Sep 95: Jeff Uphoff, NRAO.  Zapped TAB kill; done elsewhere */
/*--------------------------------------------------------------------*/
{
   char histfile[MAXAPATH];
   char xlated[MAXAPATH];
   char *rlenv = NULL;
   int llen = MAXPNAME;
   int xlen = MAXAPATH;
   int xlnb;
/*--------------------------------------------------------------------*/
   *ierr = 0;

                                        /* Check if readline disabled */
   if (((rlenv = getenv ("READLINE")) != NULL) &&
       !strcmp (rlenv, "NO")) {         /* If so, skip the rest.      */
      *ierr = 1;
      return;
   }

   if (*opcode == 1) {                  /* OPEN history.              */

      using_history ();                 /* Initialize some variables. */

      rl_readline_name = "AIPS";        /* For ~/.inputrc branching.  */

      rl_read_init_file (NULL);         /* Reread to get any AIPS-    */
                                        /* specific options/bindings. */
                                        /* (Ignore any errors...)     */

      if (history_length > 1)           /* Must be a restart...       */
         return;                        /* Don't reread file.         */

                                        /* Translate logical to path. */
      zpath_ (&llen, phname, &xlen, xlated, &xlnb, ierr);
      if (*ierr)
         return;

      sprintf (histfile, "%.*s", xlnb, xlated);
                                        /* Skip first history entry   */
                                        /* to prevent a long list of  */
                                        /* user numbers at top.       */
      *ierr = read_history_range (histfile, 1, -1);
      return;
   }

   if (*opcode == 2) {                  /* CLOS (close) history.      */

                                        /* Translate logical to path. */
      zpath_ (&llen, phname, &xlen, xlated, &xlnb, ierr);
      if (*ierr)
         return;

      sprintf (histfile, "%.*s", xlnb, xlated);
                                        /* Write out this session's   */
                                        /* (and possibly previous     */
                                        /* sessions') history.        */
      if (*ierr = write_history (histfile))
         return;

                                        /* Trim the saved history     */
                                        /* file to 500 lines.         */
      *ierr = history_truncate_file (histfile, 500);
      return;
   }

   *ierr = 1;                           /* zgrlhi() improperly called */
   return;
}

/*
 * Local Variables:
 * c-basic-offset: 3
 * comment-column: 40
 * indent-tabs-mode: nil
 * End:
 */
