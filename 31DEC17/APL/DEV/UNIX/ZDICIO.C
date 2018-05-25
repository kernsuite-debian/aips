#define Z_dicio__
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <AIPSPROTO.H>

extern FILE *dic_file;                   /* Output file               */
extern int dic_nrows;
static unsigned char buff [4096 * 3];    /* Dimension should be       */
                                         /* 3*MAXROW where MAXROW is  */
                                         /* defined in the local      */
                                         /* include for TVDIC.        */

#if __STDC__
   void zdicio_(int *lun, int *flmtyp, int *rownum, int *red,
      int *green, int *blue, int *rowlen, int *iret)
#else
   void zdicio_(lun, flmtyp, rownum, red, green, blue, rowlen, iret)
   int *lun, *flmtyp, *rownum, *red, *green, *blue, *rowlen, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! Write an image row to a Dicomed image file.                       */
/*# Z                                                                 */
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
/*   Write an image row to a Dicomed image file.  If the filmtype is  */
/*   B/W (FLMTYP = 2) then only the red channel is written.           */
/*                                                                    */
/*   Inputs:                                                          */
/*      LUN        I            Logical unit number of image file     */
/*      FLMTYP     I            Filmtype: 0 -> colour positive        */
/*                                        1 -> colour negative        */
/*                                        2 -> B/W negative           */
/*      ROWNUM     I            Number of row to write.  Bottom row   */
/*                              is ROWNUM 1.                          */
/*      RED        I(*)         Red values (0-255)                    */
/*      GREEN      I(*)         Green values (0-255)                  */
/*      BLUE       I(*)         Blue values (0-255)                   */
/*      WIDTH      I            Number of pixels in row (> 0)         */
/*                                                                    */
/*   Output:                                                          */
/*      IRET       I            Return status:                        */
/*                                 0 -> success.                      */
/*                                                                    */
/*  Generic UNIX version Ignores LUN.                                 */
/*--------------------------------------------------------------------*/
{
    long int offset;
    int i, n8 = 8;
    char msg[80];
    size_t nitems;
/*--------------------------------------------------------------------*/

    *iret = 0;

    nitems = *rowlen;
    if (*flmtyp == 2) {                  /* Black and white           */
       offset = 512 + *rowlen * (dic_nrows - *rownum);
       if (fseek(dic_file, offset, 0) != 0) {
          sprintf (msg, "ZDICIO: ERROR WRITING ROW %d\n", rownum);
          zmsgwr_ (msg, &n8);
          perror ("ZDICIO:");
          *iret = errno;
          return;
          }
       for (i = 0; i < *rowlen; i++) {
          buff[i] = (unsigned char)red[i];
          }
       if (fwrite(buff, sizeof(char), nitems, dic_file) < nitems) {
          sprintf (msg, "ZDICIO: ERROR WRITING ROW %d\n", rownum);
          zmsgwr_ (msg, &n8);
          perror ("ZDICIO:");
          *iret = errno;
          return;
          }
       }
    else {                              /* Colour                     */
       offset = 512 + 3 * *rowlen * (dic_nrows - *rownum);
       if (fseek(dic_file, offset, 0) != 0) {
          sprintf (msg, "ZDICIO: ERROR WRITING ROW %d\n", rownum);
          zmsgwr_ (msg, &n8);
          perror ("ZDICIO:");
          *iret = errno;
          return;
          }
       for (i = 0; i < *rowlen; i++) {
          buff[3*i] = (unsigned char)red[i];
          buff[3*i+1] = (unsigned char)green[i];
          buff[3*i+2] = (unsigned char)blue[i];
          }
       nitems = nitems * 3;
       if (fwrite(buff, sizeof(char), nitems, dic_file) < nitems) {
          sprintf (msg, "ZDICIO: ERROR WRITING ROW %d\n", rownum);
          zmsgwr_ (msg, &n8);
          perror ("ZDICIO:");
          *iret = errno;
          return;
          }
       }

   return;
}
