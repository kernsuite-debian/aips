      SUBROUTINE R3DTAB (FDVEC, TBIND, DPTR, NAXIS, IBLK, TAPBUF, IERR)
C-----------------------------------------------------------------------
C! Read data from FITS 3-D table and write AIPS table.
C# FITS EXT-util tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2008, 2015
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C  This routine will read the data section of a FITS 3-D table and copy
C  the data to the AIPS version of the table.
C  Inputs:
C     FDVEC    I(50)      File descriptor vector for TAPIO input
C     DPTR     I(128,2)   Data Pointers, used in table file control.
C     NAXIS    I(2)       Length of columns (in char), number of rows.
C  In/Out:
C     TBIND    I          Buffer pointer in TAPBUF
C     IBLK     I(*)       Disk Table file I/O buffer.
C     TAPBUF   I(*)       Tape I/O buffer.
C  Outputs:
C     IERR     I          Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), TBIND, DPTR(128,2), NAXIS(2), IBLK(*),
     *   TAPBUF(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION RECRD(XBPRSZ/2)
      CHARACTER CBUFF*2048
      HOLLERITH RECH(XBPRSZ)
      REAL      RECRR(XBPRSZ)
      LOGICAL   RECLL(XBPRSZ)
      INTEGER   TBYTCT, MXLREB, TLINE(XBPRSZ), ROWF, ROWL, NAXIS1,
     *   NAXIS2, LCTR, RECII(XBPRSZ), BYTCNT(7), KTYPE, I, IOFF, IT0,
     *   II, TCOUNT(128), NEXT, NBYTE, TPTYPE(128), LIMIT, TOFF(128),
     *   JT, JTRIM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECII, RECLL, RECRD, RECRR, RECH)
      DATA BYTCNT /8,4,1,4,1,2,1/
C-----------------------------------------------------------------------
      IERR = 0
      IF (NAXIS(2).EQ.0) THEN
         CALL TABIO ('CLOS', 0, LCTR, RECRR, IBLK, IERR)
         WRITE (MSGTXT,1000) ITYPE
         GO TO 990
         END IF
C                                       Calculate end & type
C                                       of Column values.
C                                       Use TFCODE to determine types
C                                       because DPTR holds output info.
      TBYTCT = 0
      DO 10 I = 1,ITNCOL
         TPTYPE(I) = MOD (TFCODE(I),10)
         TCOUNT(I) = DPTR(I,2) / 10
         TOFF(I) = DPTR(I,1)
C                                       Count bytes
         KTYPE = TPTYPE(I)
         TBYTCT = TBYTCT + TCOUNT(I) * BYTCNT(KTYPE)
 10      CONTINUE
C                                       Check buffer size
      MXLREB = XBPRSZ * NBITWD / 8
C                                       Record too big
      IF (TBYTCT.GT.MXLREB) THEN
         IERR = 5
         WRITE (MSGTXT,1010) TBYTCT, MXLREB
         GO TO 990
         END IF
C                                       Read first record,
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      NEXT = 1
      IF (IERR.NE.0) GO TO 999
C                                       Loop for all lines in table.
      NAXIS1 = NAXIS(1)
      NAXIS2 = NAXIS(2)
      ROWF = IBLK(5) + 1
      ROWL = IBLK(5) + NAXIS2
      DO 800 LCTR = ROWF,ROWL
         DO 200 I = 1,ITNCOL
C                                       Read a FITS table data entry.
            KTYPE = TPTYPE(I)
            NBYTE = BYTCNT(KTYPE) * TCOUNT(I)
            IF (KTYPE.EQ.7) NBYTE = 1 + (TCOUNT(I)-1) / 8
            CALL GTF3D (FDVEC, TBIND, NEXT, TAPBUF, NBYTE, TLINE, IERR)
            IF (IERR.NE.0) GO TO 999
C????? HOW TO DEAL WITH NULLS?????
C                                       Look for null values.
C            CALL CHCOMP (TWIDTH(I), TBCOL(I), TLINE,
C     *         1, TNULL(1,I), NULL)
C                                       Go to correct type
            IOFF = TOFF(I)
            IT0 = TPTYPE(I)
C                                       Double precision
            IF (IT0.EQ.1) THEN
               CALL ZR64RL (TCOUNT(I), 1, TLINE, RECRD(IOFF))
C                                       Single precision
            ELSE IF (IT0.EQ.2) THEN
               CALL ZR32RL (TCOUNT(I), 1, TLINE, RECRR(IOFF))
C                                       Character.
            ELSE IF (IT0.EQ.3) THEN
               CALL ZC8CL (TCOUNT(I), 1, TLINE, CBUFF)
               JT = JTRIM (CBUFF(:TCOUNT(I)))
               CALL CHR2H (TCOUNT(I), CBUFF, 1, RECH(IOFF))
C                                       Long Integer.
C                                       Convert to local I
            ELSE IF (IT0.EQ.4) THEN
               CALL ZI32IL (TCOUNT(I), 1, TLINE, RECII(IOFF))
C                                       Logical
            ELSE IF (IT0.EQ.5) THEN
               LIMIT = TCOUNT(I)
               CALL ZC8CL (LIMIT, 1, TLINE, CBUFF)
               DO 155 II = 1,LIMIT
                  RECLL(IOFF+II-1) = CBUFF(II:II).EQ.'T'
 155              CONTINUE
C                                       Short Integer.
C                                       Convert to INTEGER
            ELSE IF (IT0.EQ.6) THEN
               CALL ZI16IL (TCOUNT(I), 1, TLINE, RECII(IOFF))
C                                       bit array
            ELSE IF (IT0.EQ.7) THEN
               CALL ZX8XL (TCOUNT(I), TLINE, RECII(IOFF))
               END IF
 200        CONTINUE
C                                       Write this table row.
         CALL TABIO ('WRIT', 0, LCTR, RECRR, IBLK, IERR)
 800     CONTINUE
      CALL TABIO ('CLOS', 0, LCTR, RECRR, IBLK, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Warning: table type ',A2,' is of zero length')
 1010 FORMAT ('ERROR: TABLE ROW =',I6,' BYTES, BUFFER SIZE =',I6)
      END
