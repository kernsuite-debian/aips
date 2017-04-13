#define Z_getch__

#if __STDC__
   void zgetch_(int *ichar, char *word, int *nchar)
#else
   void zgetch_(ichar, word, nchar)
   int *ichar, *nchar ;
   char word[] ;
#endif
/*--------------------------------------------------------------------*/
/*! get a character from a REAL word                                  */
/*# Binary                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997                                         */
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
