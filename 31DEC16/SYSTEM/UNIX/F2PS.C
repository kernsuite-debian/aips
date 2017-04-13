#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
                                          /* plot parameters          */
static int   Lefts[2][4] = {{  34,  74,  36,  27 }, {  34,  96,  29,  29 }};
static int   Tops [2][4] = {{ 747, 566, 568, 568 }, { 797, 550, 552, 552 }};
static int   Maxls[2][4] = {{  97,  61,  54,  52 }, { 106,  59,  52,  47 }};
static float Delts[2][4] = {{ 6.8, 8.0, 9.0, 9.25}, { 6.6, 8.0, 9.0, 9.83}};
static int   Xoffs[4]    =  {   9,   0,   0,   0 };
static int   Yoffs[4]    =  {   0,  -8, -10, -10 };

#if __STDC__
   int main (int argc, char *argv[])
#else
   int main (argc, argv)
   int argc;
   char *argv[];
#endif
/*--------------------------------------------------------------------*/
/*! Filter Fortran printer output to Postscript                       */
/*# Hardcopy Printer                                                  */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
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
/*   and write PostScript.  Input comes from stdin and output goes to */
/*   stdout.  Once a line with the first character not = 1, 0, +, or  */
/*   blank is found, then all subsequent input is assumed non-Fortran.*/
/*   Then the only carriage control comes from page commands.         */
/*   Command line arguments: -A4 for A4 paper, -L, -M, -S for other   */
/*   print sizes.  The max # lines, mode and print size are           */
/*       -S    97      -S/-A4    95        Portrait      6.8          */
/*       -R    61      -R/-A4    58        Landscape     8.0          */
/*       -M    54      -M/-A4    52        Landscape     9.0          */
/*       -L    52      -L/-A4    47        Landscape     9.25         */
/*--------------------------------------------------------------------*/
{
   char  xxline[1024], ooline[1024];
   char  *arg;
   int   i, page, left, top, maxl, nl, Fortran, line, nr, nc, size,
         paper, duplex, xoff[2], yoff[2], Umax, j;
   float y, dy, deltay;
#if __STDC__
   int lgets (int, int *, int *, int*, char *) ;
#else
   int lgets ( ) ;
#endif
/*--------------------------------------------------------------------*/
                                          /* parameters assumed       */
      size = 0;
      paper = 0;
      duplex = 0;
      Umax = 0;
                                          /* check arg line           */
      for (i = 1; i < argc; i++) {
         arg = argv[i];
         if (arg[0] == '-') {
            if (arg[1] == 'L') size = 3;
            else if (arg[1] == 'M') size = 2;
            else if (arg[1] == 'R') size = 1;
            else if (arg[1] == 'S') size = 0;
            else if ((arg[1] == 'A') && (arg[2] == '4')) paper = 1;
            else if (arg[1] == 'd') duplex = 1;
            else if ((arg[1] >= '0') && (arg[1]  <= '9'))
               sscanf (arg, "%d", &Umax);
            }
         }
      if (Umax < 0) {
         Umax = -Umax;
         j = -1;
         for (i = 0; i < 4; i++)
             if (Umax <= Maxls[paper][i]) j = i;
         if (j >= 0) size = j;
         }
      deltay = Delts[paper][size];
      left   = Lefts[paper][size];
      top    = Tops [paper][size];
      maxl   = Maxls[paper][size];
      xoff[0] = Xoffs[size];
      yoff[0] = Yoffs[size];
      xoff[1] = Xoffs[size];
      yoff[1] = Yoffs[size];
      if (duplex) {
         xoff[0] = -Xoffs[size];
         yoff[0] = -Yoffs[size];
         }
                                          /* assume Fortran input     */
      Fortran = 1;
      dy = 1.1 * deltay;
      page = 0;
      line = 0;
                                          /* line loop                */
      while ((nr = lgets (140, &Fortran, &nl, &nc, xxline) > -1) ||
         (nc > 0)) {
                                          /* force page               */
         if ((page == 0) && (line == 0)) nl = 999;
         line = line + nl;
         if (line > maxl) nl = 999;
                                          /* page                     */
         if (nl > 900) {
            page = page + 1;
            line = 1;
                                          /* finish previous page     */
            if (page > 1) {
               printf ("stroke\n");
               printf ("vmsave restore\n");
               printf ("showpage\n");
               printf ("userdict /eop-hook known {eop-hook} if\n");
               }
                                          /* start 1st page           */
            else {
               printf ("%%!PS-Adobe-3.0\n");
               printf ("%%%%Creator: AIPS filter F2PS\n");
               printf ("%%%%Title: AIPS print out\n");
               printf ("%%%%DocumentFonts: Courier\n");
               if (size == 0)
                  printf ("%%%%Orientation: Portrait\n");
               else
                  printf ("%%%%Orientation: Landscape\n");
               printf ("%%%%EndComments\n");
               printf ("%%%%BeginProcSet: f2ps.pro\n");
               printf ("   /m {moveto} def\n");
               printf ("%%%%EndProcSet\n");
               printf ("%%%%EndProlog\n");
               printf ("userdict /start-hook known {start-hook} if\n");
               }
                                          /* rest of start page       */
            printf ("%%%%Page: %d %d\n", page, page);
            printf ("userdict /bop-hook known {bop-hook} if\n");
            printf ("/vmsave save def\n");
            if (size != 0)
               printf ("8.5 72 mul 0 translate 90 rotate\n");
            printf ("/Courier findfont %.1f scalefont setfont\n",
               deltay);
            printf ("newpath gsave\n");
            printf (" %d %d translate\n", xoff[page % 2],
               yoff[page % 2]);
            y = top;
            }
         else {
            y = y - dy * nl;
            }
                                          /* position and send        */
         printf ("%d %.3f m\n", left, y);
                                          /* escape special chars     */
         j = 0;
         for (i = 0; i < 1024 && xxline[i] != '\0'; i++) {
            if ((xxline[i] == '(') || (xxline[i] == ')') ||
               (xxline[i] == '\\')) ooline[j++] = '\\';
            ooline[j++] = xxline[i];
            }
         ooline[j] = '\0';
                                          /* show the string          */
         printf ("(%s) show\n", ooline);
         if (nr < 0) break;
         }
                                          /* finish last page         */
      printf ("stroke\n");
      printf ("vmsave restore\n");
      printf ("showpage\n");
      printf ("userdict /eop-hook known {eop-hook} if\n");
      printf ("%%%%Trailer\n");
      printf ("userdict /end-hook known {end-hook} if\n");
      printf ("%%%%EOF\n");

      return (0);
} /* end main */
#if __STDC__
   int lgets (int n, int *Fortran, int *nl, int* nc, char *xxline)
#else
   int lgets (n, Fortran, nl, nc, xxline)
   int n;
   int *Fortran, *nl, *nc;
   char *xxline;
#endif
/*--------------------------------------------------------------------*/
/*   function reads in the next input line and deduces the carriage   */
/*   control                                                          */
/*   Input:                                                           */
/*      n        I      max length of xxline                          */
/*   In/out:                                                          */
/*      Fortran  I      > 0 => Fortran carriage control               */
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
