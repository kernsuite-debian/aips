#define Z_putch__

#if __STDC__
   void zputch_(int *ichar, char *word, int *nchar)
#else
   void zputch_(ichar, word, nchar)
   int *ichar, *nchar;
   char word[];
#endif
/*--------------------------------------------------------------------*/
/*! inserts 8-bit "character" into a word                             */
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
