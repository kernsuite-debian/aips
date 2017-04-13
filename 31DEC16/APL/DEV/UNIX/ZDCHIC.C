#define Z_dchic__
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#define UMASK S_IWOTH                   /* inhibits writes by world   */
/* #define UMASK 0 */                   /* all accesses are allowed   */

int Z_nbpwd, Z_nbpab, Z_nbpb, Z_nbitwd, Z_nbitab, Z_nbitch, Z_nchlin,
   Z_nwdpdp, Z_bytflp, Z_spfrmt, Z_dpfrmt, Z_nbps, Z_nmfcb, Z_mfcb,
   Z_fcbfd, Z_fcbreq, Z_fcberr,Z_fcbxfr, Z_fcbsiz, Z_fcbsp1, Z_fcbsp2,
   Z_fcbsp3, Z_fcbsp4, Z_nintrn, Z_nfcber, Z_nfcbfd;
unsigned int Z_maskab;
char Z_systyp[5];

#if __STDC__
   void zdchic_(int *nbitwd, int *nbitch, int *nchlin, int *nwdpdp,
      int *bytflp, int *spfrmt, int *dpfrmt, int *nbps, int *nmfcb,
      int *mfcb, int *nfcber, int *nfcbfd, int *fcbfd, int *fcbreq,
      int *fcberr, int *fcbxfr, int *fcbsiz, int *fcbsp1, int *fcbsp2,
      int *fcbsp3, int *fcbsp4, int *nintrn, char systyp[],
      char sysver[])
#else
   void zdchic_(nbitwd, nbitch, nchlin, nwdpdp, bytflp, spfrmt, dpfrmt,
      nbps, nmfcb, mfcb, nfcber, nfcbfd, fcbfd, fcbreq, fcberr, fcbxfr,
      fcbsiz, fcbsp1, fcbsp2, fcbsp3, fcbsp4, nintrn, systyp, sysver)
   int *nbitwd, *nbitch, *nchlin, *nwdpdp, *bytflp, *spfrmt, *dpfrmt,
      *nbps, *nmfcb, *mfcb, *nfcber, *nfcbfd, *fcbfd, *fcbreq, *fcberr,
      *fcbxfr, *fcbsiz, *fcbsp1, *fcbsp2, *fcbsp3, *fcbsp4, *nintrn;
   char systyp[], sysver[] ;
#endif
/*--------------------------------------------------------------------*/
/*! set more system parameters; make them available to C routines     */
/*# System                                                            */
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
/*  Make various device characteristics as defined in ZDCHIN available*/
/*  to routines written in C (e.g., extern int Z_bytflp).             */
/*  Inputs:                                                           */
/*     nbitch   I   # bits per character                              */
/*     nchlin   I   # characters per input line                       */
/*     bytflp   I   Byte flip indicator code                          */
/*     spfrmt   I   Single precision floating-point format code       */
/*     dpfrmt   I   Double precision floating-point format code       */
/*     nbps     I   # bytes (i.e., AIPS-bytes) per disk sector        */
/*                     (assigned to Z_nbps as # of 8-bit bytes)       */
/*     nmfcb    I   # elements in FTAB for non-map file control blocks*/
/*     mfcb     I   # elements in FTAB for map file control blocks    */
/*     nfcber   I   Offset in non-file control blocks to error code   */
/*     nfcbfd   I   Offset in non-file control blocks to file descrip */
/*     fcbfd    I   Offset in file control blocks to file descriptor  */
/*     fcbreq   I   Offset in file control blocks to the I/O request  */
/*     fcberr   I   Offset in file control blocks to system error code*/
/*     fcbxfr   I   Offset in file control block to I/O transfer count*/
/*     fcbsiz   I   Offset in file control blocks to the file size    */
/*     fcbsp1   I   Offset in file control blocks to the 1st spare    */
/*     fcbsp2   I   Offset in file control blocks to the 2nd spare    */
/*     fcbsp3   I   Offset in file control blocks to the 3rd spare    */
/*     fcbsp4   I   Offset in file control blocks to the 4th spare    */
/*     nintrn   I   Maximum interactive POPS number                   */
/*     sysver   C*8 system version: '4.5', 'BSD 4.2', 'SYS 5', ...    */
/*     systyp   C*4 system type: 'VMS ' or 'SUN4' or ???              */
/*  Output:                                                           */
/*     nbitwd   I   # bits per word                                   */
/*     nwdpdp   I   # words per double precision floating-point       */
/*  Generic UNIX version                                              */
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per word           */
   Z_nbpwd = sizeof (int);
                                        /* Calculate # bytes per      */
                                        /* AIPS-byte once and for all.*/
   Z_nbpab = Z_nbpwd / 2;
                                        /* # bits per byte (NBBY from */
                                        /* /usr/include/sys/param.h). */
#if NBBY
   Z_nbpb = NBBY;
#else
   Z_nbpb = 8;
#endif
                                        /* # bits per word.           */
   Z_nbitwd = Z_nbpwd * Z_nbpb;
   *nbitwd = Z_nbitwd;
                                        /* # bits per AIPS-byte.      */
   Z_nbitab = Z_nbpab * Z_nbpb;
                                        /* Bit mask for masking an    */
                                        /* AIPS-byte worth of bits.   */
   Z_maskab = ~(~0 << Z_nbitab);
                                        /* # bits per character.      */
   Z_nbitch = *nbitch;
                                        /* # characters per input     */
                                        /* line.                      */
   Z_nchlin = *nchlin;
                                        /* # words per double         */
                                        /* precision floating-point   */
                                        /* (use "float" for 64-bit    */
                                        /* architectures and          */
                                        /* preprocess all double      */
                                        /* precision contructs to     */
                                        /* their single precision     */
                                        /* counterparts).             */
   if (sizeof (double) > 8)
      Z_nwdpdp = sizeof (float) / Z_nbpwd;
   else
      Z_nwdpdp = sizeof (double) / Z_nbpwd;
   *nwdpdp = Z_nwdpdp;
                                        /* Byte flip indicator code.  */
   Z_bytflp = *bytflp;
                                        /* Single precision floating- */
                                        /* point format indicator     */
                                        /* code.                      */
   Z_spfrmt = *spfrmt;
                                        /* Double precision floating- */
                                        /* point format indicator     */
                                        /* code.                      */
   Z_dpfrmt = *dpfrmt;
                                        /* Convert # AIPS-bytes per   */
                                        /* disk sector to bytes.      */
   Z_nbps = *nbps * Z_nbpab;
                                        /* # FTAB elements per non-   */
                                        /* map file control block.    */
   Z_nmfcb = *nmfcb;
                                        /* # FTAB elements per map    */
                                        /* file control block.        */
   Z_mfcb = *mfcb;
                                        /* Offset to entries in the   */
                                        /* file control blocks.       */

                                        /* I/O error: non-file        */
   Z_nfcber = *nfcber;
                                        /* File descriptor: non-file  */
   Z_nfcbfd = *nfcbfd;
                                        /* File descriptor.           */
   Z_fcbfd = *fcbfd;
                                        /* I/O transfer request.      */
   Z_fcbreq = *fcbreq;
                                        /* System error code.         */
   Z_fcberr = *fcberr;
                                        /* I/O transfer count.        */
   Z_fcbxfr = *fcbxfr;
                                        /* File size.                 */
   Z_fcbsiz = *fcbsiz;
                                        /* 1st spare entry.           */
   Z_fcbsp1 = *fcbsp1;
                                        /* 2nd spare entry.           */
   Z_fcbsp2 = *fcbsp2;
                                        /* 3rd spare entry.           */
   Z_fcbsp3 = *fcbsp3;
                                        /* 4th spare entry.           */
   Z_fcbsp4 = *fcbsp4;
                                        /* Number of interactive AIPS */
   Z_nintrn = *nintrn;
                                        /* system type                */
   Z_systyp[0] = systyp[0] ;
   Z_systyp[1] = systyp[1] ;
   Z_systyp[2] = systyp[2] ;
   Z_systyp[3] = systyp[3] ;
   Z_systyp[4] = '\0' ;

                                        /* permissions mask set       */
   umask (UMASK);

   return;
}
