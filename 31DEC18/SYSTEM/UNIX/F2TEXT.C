#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#if __STDC__
   int main (int argc, char *argv[])
#else
   int main (argc, argv)
   int argc;
   char *argv[];
#endif
/*--------------------------------------------------------------------*/
/*! Filter Fortran printer output to Plain Text                       */
/*# Hardcopy Printer                                                  */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1996                                               */
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
/*   Program to filter Fortran-style printer output (or non-Fortran)  */
/*   and write Plain text.  As with any filter, it takes input from   */
/*   stdin, processes it and diverts output to stdout.  There are no  */
/*   stderr messages in the program at this point.                    */
/*   Once a line with the first character not = 1, 0, +, or blank is  */
/*   found, then all subsequent input is assumed to be non-Fortran.   */
/*   Then the only carriage control comes from page commands.         */
/*   NOTE: The '+' in column 1 will NOT cause overprinting.           */
/*   Command line args: -nn where nn is the number of lines/page.     */
/*                      -e forces last page to eject via formfeed     */
/*--------------------------------------------------------------------*/
{
   char  xxline[140];
   char  *arg;
   int   i, page, nl, Fortran, line, nr, nc, Umax, eject;
#if __STDC__
   int lgets (int, int *, int *, int*, char *) ;
#else
   int lgets ( ) ;
#endif
/*--------------------------------------------------------------------*/
                                          /* parameters assumed       */
   Umax = 61;
   eject=0;
                                          /* check arg line           */
   for (i = 1; i < argc; i++) {
      arg = argv[i];
      if (arg[0] == '-') {
                                          /* Command line args are    */
                                          /* parsed here via arg[1]   */
         if (arg[1] == 'e') {
            eject=1;
         } else if ((arg[1] >= '0') && (arg[1]  <= '9')) {
            sscanf (arg, "%d", &Umax);
                                        /* We got the "-" too */
            Umax = -Umax;
         }
      }
   }
                                          /* assume Fortran input     */
   Fortran = 1;
   page = 0;
   line = 0;
   if (Umax > 100) Umax=100;
   if (Umax < 10) Umax=10;
                                          /* line loop                */
   while ((nr = lgets (140, &Fortran, &nl, &nc, xxline) > -1) ||
          (nc > 0)) {
                                          /* force page               */
      if ((page == 0) && (line == 0)) nl = 999;
      line = line + nl;
      if (line > Umax) nl = 999;
                                          /* page or #lines reached   */
      if (nl > 900) {
         page = page + 1;
         line = 1;
         nl = 1;
                                          /* no leading blank pages   */
         if (page > 1) printf ("\f");
                                          /* skip 2 lines             */
      } else if (nl == 2) {
         printf ("\n");
      }
                                          /* treat '+' like a space   */
      printf ("%s\n", xxline);
      if (nr < 0) break;
   }
                                          /* finish last page         */
   if (eject == 1) printf ("\f");
   return (0);
} /* end main */
#if __STDC__
   int lgets (int n, int *Fortran, int *nl, int* nc, char *xxline)
#else
   int lgets (n, Fortran, nl, nc, xxline)
   int n, *Fortran, *nl, *nc;
   char *xxline;
#endif
/*--------------------------------------------------------------------*/
/*   function reads in the next input line and deduces the carriage   */
/*   control                                                          */
/*   Input:                                                           */
/*      n        I      max length of xxline                          */
/*   Output:                                                          */
/*      nl       I      number lines to skip as control: 999 -> page  */
/*      nc       I      number chars in xxline                        */
/*      xxline   C(*)   input text line                               */
/*      lgets    I      -1 => EOF terminates, 0 => \n terminates,     */
/*                      +1 => ran out of room (long input line)       */
/*--------------------------------------------------------------------*/
{
   register int c;
   register char *cs;
   int  nr;
/*--------------------------------------------------------------------*/
      cs = xxline;
      *nc = 0;
      *nl = 1;
      nr = 0;
                                              /* 1st char             */
      c = getc (stdin);
      if ((c != EOF) && (c != '\n')) {
         if (*Fortran > 0) {
            if (c == '1')
               *nl = 999;
            else if (c == '0')
               *nl = 2;
            else if (c == ' ')
               *nl = 1;
            else if (c == '+')
               *nl = 0;
            else
               *Fortran = 0;
            }
         if (*Fortran <= 0) {
            if (c == '\f')
               *nl = 999;
            else {
               *nc = 1;
               *cs++ = c;
               }
            }
                                              /* remaining chars      */
         while ((*nc < n) && ((c = getc (stdin)) != EOF)) {
            if (c == '\n') break;
            *nc = *nc + 1;
            *cs++ = c;
            }
         }
                                             /* null terminate        */
      *cs = '\0';
                                             /* return codes          */
      if (c == EOF)
         return (-1);
      else if (c == '\n')
         return (0);
      else
         return (1);
}
