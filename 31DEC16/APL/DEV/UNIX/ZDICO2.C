#define Z_dico2__
#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

char *dic_filename;                      /* File name (used by        */
                                         /* ZDICCL).                  */
                                         /* NOTE: storage must be     */
                                         /* malloc'ed or segmentation */
                                         /* violations will occur in  */
                                         /* zdicc2_ under Sun OS.     */
FILE* dic_file;                          /* File stream (used by      */
                                         /* ZDICIO and ZDICCL)        */
int dic_nrows;                           /* number of rows (used by   */
                                         /* ZDICIO)                   */
int dic_called = 0;                      /* Set to 1 when first       */
                                         /* called.                   */
static char *sizes[3] = {"35mm", "2x2", "4x5"};
static char *types[3] = {"ColPos", "ColNeg", "B/W"};
static char *mon[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

#if __STDC__
   void zdico2_(char *sysnam, int *slen, int *userno, int *flmsiz,
      int *flmtyp, int *copies, int *bppix, int *width, int *height,
      char *commnt, int *clen, int *iret)
#else
   void zdico2_(sysnam, slen, userno, flmsiz, flmtyp, copies, bppix,
      width, height, commnt, clen, iret)
   char *sysnam, *commnt;
   int  *slen, *userno, *flmsiz, *flmtyp, *copies, *bppix, *width,
      *height, *clen, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! Open a Dicomed image file and fill in the header                  */
/*# Z2                                                                */
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
/*   Open a Dicomed image file and fill in the header.                */
/*   Inputs:                                                          */
/*      SYSNAM    C*(*)       System name.                            */
/*      SLEN      I           Length of system name excluding         */
/*                            trailing blanks.                        */
/*      USERNO    I           AIPS user number.                       */
/*      FLMSIZ    I           Film size: 0 -> 35mm                    */
/*                                       1 -> 2"x2"                   */
/*                                       2 -> 4"x5"                   */
/*      FLMTYP    I           Film type: 0 -> colour positive         */
/*                                       1 -> colour negative         */
/*                                       2 -> B/W negative            */
/*      COPIES    I           Number of copies to make.               */
/*      BPPIX     I           Number of bits/pixel.                   */
/*      WIDTH     I           Image width in pixels.                  */
/*      HEIGHT    I           Image height in pixels.                 */
/*      COMMNT    C*(*)       User's comment.                         */
/*      CLEN      I           Length of user's comment excluding      */
/*                            trailing blanks.                        */
/*                                                                    */
/*   Output:                                                          */
/*      IRET      I           Return status: 0 -> success             */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    int i, n8 = 8;
    time_t t;
    struct tm *current_time;
    char system[9], comment[49], msg[80];
    char *start, *tptr;
/*--------------------------------------------------------------------*/
                                        /* Get creation time:         */
    t = time(0);
    current_time = localtime(&t);
                                        /* Assume success:            */
    *iret = 0;
                                        /* Check that a Dicomed file  */
                                        /* is not already open:       */
    if (dic_called) {
       sprintf (msg, "ZDICO2: A DICOMED FILE IS ALREADY OPEN\n");
       zmsgwr_ (msg, &n8);
       *iret = 1;
       return;
       }
    else {
       dic_called = 1;
       }
                                        /* Save useful information:   */
    dic_nrows  = *height;
    dic_filename = (char *)malloc(24);
                                        /* This form of filename is   */
                                        /* required by NRAO software  */
    sprintf (dic_filename, "/tmp/%04d_%03d_%02d%02d%02d_V1", *userno,
       current_time->tm_yday+1, current_time->tm_hour,
       current_time->tm_min, current_time->tm_sec);

                                        /* Convert strings to C form  */
    for (i = 0; i < *slen && i < 8 && sysnam[i] != '.'; i++)  {
       system[i] = sysnam[i];
       }
    system[i] = '\0';
    for (i = 0; i < *clen && i < 48; i++) {
       comment[i] = commnt[i];
       }
    comment[i] = '\0';
                                       /* Open file:                  */
    if ((dic_file = fopen(dic_filename, "w")) == 0) {
       sprintf (msg, "ZDICO2: ERROR OPENING DICOMED FILE\n");
       zmsgwr_ (msg, &n8);
       perror("ZDICO2:");
       *iret = errno;
       return;
       }
                                       /* Extract file base name:     */
    start = tptr = dic_filename;
    while (*tptr != '\0') {
       if (*(tptr++) == '/') start = tptr;
       }

                                       /* Write header:              */
    errno = 0;
    fprintf(dic_file, "Filename: %-18s", start);
    fprintf(dic_file, "            ");
    fprintf(dic_file, "Origin: %-8s       \n", system);
    fprintf(dic_file, "Date: %02d-%3s-%02d     ",
            current_time->tm_mday, mon[current_time->tm_mon],
            current_time->tm_year);
    fprintf(dic_file, "Time: %02d:%02d:%02d      ",
            current_time->tm_hour, current_time->tm_min,
            current_time->tm_sec);
    fprintf(dic_file, "Userno: %4d           \n", *userno);
    fprintf(dic_file, "Filmsize: %-4s      ", sizes[*flmsiz]);
    fprintf(dic_file, "Filmtype: %-6s    ", types[*flmtyp]);
    fprintf(dic_file, "Copies: %3d            \n", *copies);
    fprintf(dic_file, "Bits/pixel: %2d      ", *bppix);
    fprintf(dic_file, "Columns: %4d       ", *width);
    fprintf(dic_file, "Rows: %4d             \n", *height);
    fprintf(dic_file, "LUTs/pixelbyte: 0   ");
    fprintf(dic_file, "Bits/LUTelement:  0 ");
    fprintf(dic_file, "OFMelements:     0     \n");
    fprintf(dic_file, "LUTpointer:     0   ");
    fprintf(dic_file, "OFMpointer:     0   ");
    fprintf(dic_file, "Datapointer:   512     \n");
    fprintf(dic_file, "Usercomments: %-49s\n", comment);
    fprintf(dic_file, "Recorded: %53s\n", " ");
    if (errno != 0) {
       sprintf (msg, "ZDICO2: ERROR WRITING HEADER\n");
       zmsgwr_ (msg, &n8);
       perror("ZDICO2:");
       }

   return;
}
