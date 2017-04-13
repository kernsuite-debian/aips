#define Z_sssx2__
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <AIPSPROTO.H>
#define CLOSE   12     /* Close the XAS, allows new connections       */
#define XDIE    18     /* close down XAS and lock servers             */

#define PACKSIZE (65536+6)
#define NUMOP 83       /* Largest opcode                              */

#if __STDC__
   void zsssx2_ (int *fcb, int *op, short int *opswap, short int dat[],
      int *ntbyte, char buffer[], int *nsbyte, int *istat)
#else
   void zsssx2_ (fcb, op, opswap, dat, ntbyte, buffer, nsbyte, istat)
   int *fcb, *op, *ntbyte, *nsbyte, *istat;
   short int *opswap, dat[];
   char buffer[];
#endif
/*--------------------------------------------------------------------*/
/*! write to/ read from SUN-Screen-Server device                      */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1998, 2008                                    */
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
/*  ZSSSX2 - send data and receive status from the sss "device".      */
/*  Input                                                             */
/*     fcb    I       file descriptor to write to                     */
/*     op     I       opcode                                          */
/*     opswap I*2     opcode swapped                                  */
/*     dat    I*2(4)  data                                            */
/*     ntbyte I       number of words of buffer data to send          */
/*  Output                                                            */
/*     nsbyte I       number of words of data returned (often 0)      */
/*     istat  I       status returned, 0 == success normally          */
/*                       333,444 = write, read error                  */
/*                       else returned by XAS                         */
/*  In/out                                                            */
/*     buffer I*2(*)  on input contains ntbyte of I*2 data to send    */
/*                    (if any), on return contains nsbyte of variable */
/*                    length data, if any.                            */
/*                    data sent should have values <= 255 since they  */
/*                    are sent in byte form (IMRD is sent as word)    */
/*                    data returned in word form can have larger value*/
/*--------------------------------------------------------------------*/
{
   extern int buffered;
   extern short int packet_int[PACKSIZE];
   extern int pack_offset;
   extern int bufferop[NUMOP+1]; /* bufferop[OPCODE] >0 --> no status */
                                 /* return if buffered                */
   extern int Z_bytflp, Z_sndbufs, Z_rcvbufs, Z_sndinis, Z_rcvinis;
   int size_i2 = sizeof(short int);
   static unsigned char *packet_byte = (unsigned char *)packet_int;
   unsigned char *pb, *pp;
   int buflen, lbytes, nbytes, mbytes, lwords, optlen, n7 = 7, n2 = 2;
   char msg[80];
   register int i;
   short int rdat[4] ;
   union {
      short int shortint;
      struct {
         unsigned char lo, hi;
         }byte;
      }x;
   short int flipped;
/*--------------------------------------------------------------------*/
   *istat = 0;
                                        /* write full buffer          */
   if (buffered)
      if ((pack_offset+6+*ntbyte/sizeof(short int)+1) > PACKSIZE) {
         buflen = pack_offset*size_i2;
         nbytes = buflen;
                                        /* increase buffer size       */
         if ((Z_sndbufs == Z_sndinis) && (nbytes > Z_sndinis)) {
            optlen = sizeof (Z_sndbufs);
            Z_sndbufs = 8192;
            if (nbytes > Z_sndbufs) Z_sndbufs = 2 * nbytes;
            if (setsockopt (*fcb, SOL_SOCKET, SO_SNDBUF,
               (char *) &Z_sndbufs, optlen) < 0) {
               sprintf (msg, "ZSSSX2 increase sndbuf size %s",
                  strerror(errno));
               zmsgwr_ (msg, &n7);
               Z_sndbufs = Z_sndinis+1;
               }
            else
               if (Z_sndinis > 0) {
                  sprintf (msg, "ZSSSX2 increased send buffer size");
                  zmsgwr_ (msg, &n2);
               }
            }
                                        /* write buffer               */
         pp = (unsigned char *) packet_int;
         for (nbytes = buflen; nbytes > 0; nbytes -= mbytes) {
            lbytes = nbytes;
            mbytes = write (*fcb, pp+(buflen-nbytes), lbytes);
            if (mbytes <= 0) {
               sprintf (msg, "ZSSSX2: write packet %s", strerror(errno));
               zmsgwr_ (msg, &n7);
               *istat = 333;
               pack_offset = 0;
               goto exit;
               }
            }
         pack_offset = 0;
         }
                                        /* 1st write opcode, then dat */
   packet_int[pack_offset] = *(short *) opswap;
   for (i=0; i<4; i++)
      packet_int[i+1+pack_offset] = dat[i];
                                       /* convert length to FITS I*2 */
   pb = packet_byte + 6*size_i2 + pack_offset*size_i2;
   if ((Z_bytflp == 1) || (Z_bytflp == 3)) {
      x.shortint = *ntbyte;
      flipped = x.byte.lo << 8 | x.byte.hi ;
      packet_int[pack_offset+5] = flipped;
      }
   else {
      packet_int[pack_offset+5] = *ntbyte;
      }
                                        /* copy buffer                */
   for (i=0; i<*ntbyte; i++) *pb++ = (unsigned char)buffer[i];
                                        /* write even number bytes    */
   lwords = *ntbyte + (*ntbyte)%2;
   pack_offset = pack_offset + 6 + lwords/size_i2;
   if (!bufferop[*op]) {
      buflen = pack_offset*size_i2;
                                        /* increase buffer size       */
      nbytes = buflen;
      if ((Z_sndbufs == Z_sndinis) && (nbytes > Z_sndinis)) {
         optlen = sizeof (Z_sndbufs);
         Z_sndbufs = 8192;
         if (nbytes > Z_sndbufs) Z_sndbufs = 2 * nbytes;
         if (setsockopt (*fcb, SOL_SOCKET, SO_SNDBUF,
            (char *) &Z_sndbufs, optlen) < 0) {
            sprintf (msg, "ZSSSX2 increase sndbuf size %s",
               strerror(errno));
            zmsgwr_ (msg, &n7);
            Z_sndbufs = Z_sndinis+1;
            }
         else
            if (Z_sndinis > 0) {
               sprintf (msg, "ZSSSX2 increased send buffer size");
               zmsgwr_ (msg, &n2);
            }
         }
      pp = (unsigned char *) packet_int;
      for (nbytes = buflen; nbytes > 0; nbytes -= mbytes) {
         lbytes = nbytes;
         mbytes = write (*fcb, pp+(buflen-nbytes), lbytes);
         if (mbytes <= 0) {
            sprintf (msg, "ZSSSX2: write data packet 2 %s",
               strerror(errno));
            zmsgwr_ (msg, &n7);
            *istat = 333;
            pack_offset = 0;
            goto exit;
            }
         }
      pack_offset = 0;
      }
                                        /* Then read the return info  */
                                        /* if not a buffered opcode   */
   if (!bufferop[*op]) {
      buflen = 2 * size_i2;
      pp = (unsigned char *) rdat;
      for (nbytes = buflen; nbytes > 0; nbytes -= mbytes) {
         lbytes = nbytes;
         mbytes = read (*fcb, pp+(buflen-nbytes), lbytes);
         if (mbytes <= 0) {
            if ((*op != CLOSE) && (*op != XDIE)) {
               sprintf (msg, "ZSSSX2: read error return %s",
                  strerror(errno));
               zmsgwr_ (msg, &n7);
               *istat = 444;
               }
            *nsbyte = 0;
            pack_offset = 0;
            goto exit;
            }
         }
                                         /* convert length and status  */
      *nsbyte = *(rdat+1);
      *istat = *rdat;
      if ((Z_bytflp == 1) || (Z_bytflp == 3)) {
         x.shortint = *nsbyte;
         flipped = x.byte.lo << 8 | x.byte.hi ;
         *nsbyte = flipped;
         x.shortint = *istat;
         flipped = x.byte.lo << 8 | x.byte.hi ;
         *istat = flipped;
         }

      buflen = *nsbyte ;
                                        /* increase buffer size       */
      nbytes = buflen;
      if ((Z_rcvbufs == Z_rcvinis) && (nbytes > Z_rcvinis)) {
         optlen = sizeof (Z_rcvbufs);
         Z_rcvbufs = 16384;
         if (nbytes > Z_rcvbufs) Z_rcvbufs = 2 * nbytes;
         if (setsockopt (*fcb, SOL_SOCKET, SO_RCVBUF,
            (char *) &Z_rcvbufs, optlen) < 0) {
            sprintf (msg, "ZSSSX2 increase rcvbuf size %s",
               strerror(errno));
            zmsgwr_ (msg, &n7);
            Z_rcvbufs = Z_rcvinis+1;
            }
         else
            if (Z_rcvinis > 0) {
               sprintf (msg, "ZSSSX2 increased receive buffer size");
               zmsgwr_ (msg, &n2);
               }
         }
      pp = (unsigned char *) buffer;
      for (nbytes = buflen; nbytes > 0; nbytes -= mbytes) {
         lbytes = nbytes;
         mbytes = read (*fcb, pp+(buflen-nbytes), lbytes);
         if (mbytes <= 0) {
            sprintf (msg, "ZSSSX2: read data %s", strerror(errno));
            zmsgwr_ (msg, &n7);
            *istat = 444;
            pack_offset = 0;
            goto exit;
            }
         }
      }
   else {
      *istat = 0;
      *nsbyte= 0;
      }

exit:
   return;
}
