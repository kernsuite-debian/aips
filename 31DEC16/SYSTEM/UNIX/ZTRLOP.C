#define Z_trlo2__
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <AIPSPROTO.H>

#if __STDC__
   void ztrlo2_(int *llen, char *lognam, int *xlen, char *xlated,
      int *xlnb, int *ierr)
#else
   void ztrlo2_(llen, lognam, xlen, xlated, xlnb, ierr)
   int *llen, *xlen, *xlnb, *ierr;
   char lognam[], xlated[];
#endif
/*--------------------------------------------------------------------*/
/*! translate a logical name + other C routines for PP.FOR            */
/*# System Z2                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2000-2001, 2004, 2012                  */
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
/*  Translate a logical name (i.e., environment variable).            */
/*  NOTE: This routine is ONLY for use by other Z-routines.           */
/*  Inputs:                                                           */
/*     llen     I          Length of "lognam" (1-relative)            */
/*     lognam   H*(llen)   Logical name (must be null, blank or colon */
/*                         terminated)                                */
/*     xlen     I          Length of "xlated" (1-relative)            */
/*  Output:                                                           */
/*     xlated   H*(xlen)   Translation (blank filled)                 */
/*     xlnb     I          Position of last non-blank in "xlated"     */
/*                         (1-relative)                               */
/*     ierr     I          Error return code: 0 => no error           */
/*                             1 => error                             */
/*  Generic UNIX version - uses "getenv"                              */
/*--------------------------------------------------------------------*/
{
   char envvar[MAXPNAME+1], envval[MAXAPATH+1];
   register int i;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* logical name (i.e.,        */
                                        /* environment variable).     */
                                        /* Terminate on non-          */
                                        /* alphanumeric character.    */
   for (i = 0; i < *llen && i < MAXPNAME && (isalpha (lognam[i]) ||
      isdigit (lognam[i]) || lognam[i] == '_'); i++)
      envvar[i] = lognam[i];
   envvar[i] = '\0';
                                        /* Get value of environment   */
                                        /* variable (if any).         */
   *xlnb = 0;
   envval[0] = '\0';
   if (getenv (envvar) == 0)
      *ierr = 1;
   else {
      sprintf (envval, "%s", getenv (envvar));
      if (envval[0] == '\0')
         *ierr = 1;
      else {
                                        /* Copy translation (up to    */
                                        /* first null) into "xlated". */
         for (i = 0; i < *xlen && i < MAXAPATH && envval[i] != '\0'; i++)
            xlated[i] = envval[i];
                                        /* 1-relative position of     */
                                        /* last non-blank in "xlated".*/
         *xlnb = i;
                                        /* Blank fill remainder of    */
                                        /* "xlated".                  */
         for (i = *xlnb; i < *xlen ; i++)
            xlated[i] = ' ';
         }
      }

   return;
}
#define Z_getch__

#if __STDC__
   void zgetch_(int *ichar, char *word, int *nchar)
#else
   void zgetch_(ichar, word, nchar)
   int *ichar, *nchar ;
   char word[] ;
#endif
/*--------------------------------------------------------------------*/
/*  Extracts the character in position "nchar" of the REAL argument   */
/*  "word" and inserts it in the least significant bits of the        */
/*  INTEGER argument "ichar" with zero in the rest.  It should also   */
/*  work for INTEGER "word" as long as "nchar" is valid.  Characters  */
/*  are numbered from 1 in the order in which they would be printed   */
/*  by a Fortran "A" format specifier.                                */
/*  NOTE - we actually get 8 bits here - so works for bytes too       */
/*  Inputs:                                                           */
/*     word   R   Word from which the character is to be extracted    */
/*     nchar  I   Position of character to extract                    */
/*  Output:                                                           */
/*     ichar  I   Extracted character in LS bits, zero in the rest    */
/*  Generic UNIX version - removed error testing for speed            */
/*--------------------------------------------------------------------*/
{
                                        /* Get the "nchar" character  */
                                        /* (1-relative) of "word" and */
                                        /* store it in the least      */
                                        /* significant bits of        */
                                        /* "ichar".                   */
   *ichar = word[*nchar - 1];
   if (*ichar < 0) *ichar = *ichar + 256;

   return;
}
#define Z_putch__

#if __STDC__
   void zputch_(int *ichar, char *word, int *nchar)
#else
   void zputch_(ichar, word, nchar)
   int *ichar, *nchar;
   char word[];
#endif
/*--------------------------------------------------------------------*/
/*  Inserts the character contained in the least significant bits of  */
/*  the INTEGER argument "ichar" into the "nchar" position of the     */
/*  REAL argument "word".  It should also work for INTEGER "word" as  */
/*  long as "nchar" is valid.  Characters are numbered from 1 in the  */
/*  order in which they would be printed by a Fortran "A" format      */
/*  specifier.                                                        */
/*  Inputs:                                                           */
/*     ichar   I     Character to insert in LS bits                   */
/*     nchar   I     Position in "word" to store character            */
/*  Output:                                                           */
/*     word    R/I   Word into which character is to be inserted      */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   int lword ;
                                        /* Store the character held   */
                                        /* in the least significant   */
                                        /* bits of an otherwise zero  */
                                        /* "ichar" in the "nchar"     */
                                        /* (1-relative) character     */
                                        /* position of "word".        */
   lword = *ichar ;
   if (lword > 127) lword = lword - 256 ;
   word[*nchar - 1] = lword;

   return;
}

#if __STDC__
   void zadrsz_(int *llen)
#else
   void zadrsz_(llen)
   int *llen;
#endif
/*--------------------------------------------------------------------*/
/*  returns the sizeof an pointer variable                            */
/*--------------------------------------------------------------------*/
{
  *llen = sizeof(int *);

  return;
}
#define Z_exit__

#if __STDC__
   void zexit_ (int *a)
#else
   void zexit_ (a)
   int *a;
#endif
/*--------------------------------------------------------------------*/
/*   Task termination returning status code to calling procedure      */
/*   Inputs:                                                          */
/*      a      I    status code: 0 no error, > 0 error                */
/*   Replaces CALL EXIT(n) in old FORTRANs                            */
/*--------------------------------------------------------------------*/
{
   exit (*a) ;
   return ;
}
